//! Play pool — per-(game_id, team, token) bag of real historical plays.
//!
//! At serving time the token classifier still decides *which* token a play is,
//! but the concrete field values are realized by sampling a real historical
//! play of that token (scoped to the offense team), instead of drawing
//! uniformly from the token's [lo, hi] bucket. The pool is materialized offline
//! by `scripts/materialize_play_pool.py` and passed in flat from Python at
//! engine-init time, exactly like the online feature store.
//!
//! ## Row-index sampling
//!
//! A bag is *not* a flat list of yards — it is a small column store
//! (`PlayBag`), one `Vec<i16>` per configured pool field (`[play_pool].fields`
//! in pipeline.toml), all the same length. The hot loop samples a single row
//! index and reads every field at that index, so the values it pulls always
//! come from one real snap and stay mutually coherent (yards now; time,
//! receiver, … later). The equal-length invariant across a bag's fields is what
//! makes "sample one index" well-defined — `PlayBag::new` asserts it.
//!
//! Nested maps (gid -> team -> token -> bag) so a hot-loop lookup keys on
//! `&str` with no per-lookup allocation.

use rustc_hash::FxHashMap;

/// One token's sampled plays as a column store, split by type: `numeric[f]` /
/// `strings[f]` hold field `f`'s values for that lane, positionally (the caller
/// addresses a column by index). All columns across both lanes share one
/// length: row `i` is the same real play everywhere.
pub struct PlayBag {
    numeric: Vec<Vec<i16>>,
    strings: Vec<Vec<String>>,
}

impl PlayBag {
    /// Build from one column per field per lane, asserting the equal-length
    /// invariant that row-index sampling relies on.
    fn new(numeric: Vec<Vec<i16>>, strings: Vec<Vec<String>>) -> Self {
        let len = numeric
            .first()
            .map(|c| c.len())
            .or_else(|| strings.first().map(|c| c.len()))
            .unwrap_or(0);
        for c in &numeric {
            assert_eq!(c.len(), len, "play pool bag has ragged numeric columns");
        }
        for c in &strings {
            assert_eq!(c.len(), len, "play pool bag has ragged string columns");
        }
        PlayBag { numeric, strings }
    }

    /// Number of sampled plays in this bag.
    #[inline]
    pub fn len(&self) -> usize {
        self.numeric
            .first()
            .map(|c| c.len())
            .or_else(|| self.strings.first().map(|c| c.len()))
            .unwrap_or(0)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Numeric value of column `field_idx` for the play at row `row`.
    #[inline]
    pub fn num(&self, field_idx: usize, row: usize) -> i16 {
        self.numeric[field_idx][row]
    }

    /// String value of column `field_idx` for the play at row `row`.
    #[inline]
    pub fn text(&self, field_idx: usize, row: usize) -> &str {
        &self.strings[field_idx][row]
    }
}

#[derive(Default)]
pub struct PlayPool {
    map: FxHashMap<String, FxHashMap<String, FxHashMap<String, PlayBag>>>,
}

impl PlayPool {
    /// Build from parallel key arrays + field-major values, one set per lane.
    ///
    /// `game_ids[k]`, `teams[k]`, `tokens[k]` identify the pool key whose
    /// contents are `num_values[f][k]` / `str_values[f][k]` for each field `f`
    /// of that lane. (These are the columns of the materialized
    /// `play_pool.parquet`, handed over flat from Python; field-major mirrors
    /// "one parquet list-column per field".)
    pub fn new(
        game_ids: &[String],
        teams: &[String],
        tokens: &[String],
        num_values: Vec<Vec<Vec<i16>>>,
        str_values: Vec<Vec<Vec<String>>>,
    ) -> Self {
        let n = game_ids.len();
        assert_eq!(teams.len(), n);
        assert_eq!(tokens.len(), n);
        for col in &num_values {
            assert_eq!(
                col.len(),
                n,
                "each numeric column must cover every pool key"
            );
        }
        for col in &str_values {
            assert_eq!(col.len(), n, "each string column must cover every pool key");
        }
        let n_num = num_values.len();
        let n_str = str_values.len();

        let mut map: FxHashMap<String, FxHashMap<String, FxHashMap<String, PlayBag>>> =
            FxHashMap::default();
        for k in 0..n {
            // Gather this key's bag: one column per field per lane, pulled
            // across the field-major lane arrays.
            let numeric: Vec<Vec<i16>> = (0..n_num).map(|f| num_values[f][k].clone()).collect();
            let strings: Vec<Vec<String>> = (0..n_str).map(|f| str_values[f][k].clone()).collect();
            map.entry(game_ids[k].clone())
                .or_default()
                .entry(teams[k].clone())
                .or_default()
                .insert(tokens[k].clone(), PlayBag::new(numeric, strings));
        }
        PlayPool { map }
    }

    /// The play bag for `(gid, team, token)`, or `None` if no pool exists.
    #[inline]
    pub fn get(&self, gid: &str, team: &str, token: &str) -> Option<&PlayBag> {
        self.map.get(gid)?.get(team)?.get(token)
    }
}

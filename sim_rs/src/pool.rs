//! Play pool — per-(game_id, team, token) bag of real historical yards.
//!
//! At serving time the token classifier still decides *which* token a play is,
//! but the concrete yards are realized by sampling a real historical play of
//! that token from this pool (scoped to the offense team), instead of drawing
//! uniformly from the token's [lo, hi] bucket. The pool is materialized offline
//! by `scripts/materialize_play_pool.py` and passed in flat from Python at
//! engine-init time, exactly like the online feature store.
//!
//! Nested maps (gid -> team -> token -> yards) so a hot-loop lookup keys on
//! `&str` with no per-lookup allocation.

use rustc_hash::FxHashMap;

#[derive(Default)]
pub struct PlayPool {
    map: FxHashMap<String, FxHashMap<String, FxHashMap<String, Vec<i16>>>>,
}

impl PlayPool {
    /// Build from parallel key arrays + one yards bag per key.
    ///
    /// `game_ids[i]`, `teams[i]`, `tokens[i]` identify the pool whose contents
    /// are `yards[i]`. The four inputs are the columns of the materialized
    /// `play_pool.parquet`, handed over flat from Python.
    pub fn new(
        game_ids: &[String],
        teams: &[String],
        tokens: &[String],
        yards: Vec<Vec<i16>>,
    ) -> Self {
        let n = game_ids.len();
        assert_eq!(teams.len(), n);
        assert_eq!(tokens.len(), n);
        assert_eq!(yards.len(), n);

        let mut map: FxHashMap<String, FxHashMap<String, FxHashMap<String, Vec<i16>>>> =
            FxHashMap::default();
        for (i, bag) in yards.into_iter().enumerate() {
            map.entry(game_ids[i].clone())
                .or_default()
                .entry(teams[i].clone())
                .or_default()
                .insert(tokens[i].clone(), bag);
        }
        PlayPool { map }
    }

    /// The yards bag for `(gid, team, token)`, or `None` if no pool exists.
    #[inline]
    pub fn get(&self, gid: &str, team: &str, token: &str) -> Option<&[i16]> {
        self.map
            .get(gid)?
            .get(team)?
            .get(token)
            .map(|v| v.as_slice())
    }
}

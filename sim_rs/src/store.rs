//! Online feature store — columnar (game_id, team) -> online features.
//!
//! Built by handing in raw arrays from Python (keys + values). We don't read
//! parquet from Rust yet; the Python training/offline side still owns writing
//! features.parquet, and at engine-init time we just pass over the materialized
//! columns.

use rustc_hash::FxHashMap;

pub struct OnlineStore {
    /// (game_id, team) -> row index into `matrix`
    pub key_index: FxHashMap<(String, String), usize>,
    /// row-major: n_keys rows × n_feats cols
    pub matrix: Vec<f32>,
    pub n_feats: usize,
    /// feature name -> column in `matrix`
    pub feat_col: FxHashMap<String, usize>,
    /// game_id -> (home_team, away_team)
    pub meta: FxHashMap<String, (String, String)>,
}

impl OnlineStore {
    pub fn new(
        game_ids: &[String],
        teams: &[String],
        home_teams: &[String],
        away_teams: &[String],
        feat_names: &[String],
        values: &[f32], // row-major (n_keys, n_feats)
    ) -> Self {
        let n_keys = game_ids.len();
        let n_feats = feat_names.len();
        assert_eq!(values.len(), n_keys * n_feats);

        let mut key_index = FxHashMap::default();
        let mut meta = FxHashMap::default();
        for i in 0..n_keys {
            key_index.insert((game_ids[i].clone(), teams[i].clone()), i);
            meta.entry(game_ids[i].clone())
                .or_insert_with(|| (home_teams[i].clone(), away_teams[i].clone()));
        }

        let mut feat_col = FxHashMap::default();
        for (c, f) in feat_names.iter().enumerate() {
            feat_col.insert(f.clone(), c);
        }

        OnlineStore {
            key_index,
            matrix: values.to_vec(),
            n_feats,
            feat_col,
            meta,
        }
    }

    #[inline]
    pub fn row_idx(&self, gid: &str, team: &str) -> usize {
        // TODO: avoid the String clone on hot path — intern game_id/team to u32.
        *self.key_index
            .get(&(gid.to_string(), team.to_string()))
            .expect("online key missing")
    }

    /// Gather `cols` for every `row_idxs[i]` into `out[i, :]`.
    #[inline]
    pub fn gather(&self, row_idxs: &[usize], cols: &[usize], out: &mut [f32]) {
        let k = cols.len();
        for (i, &r) in row_idxs.iter().enumerate() {
            let base = r * self.n_feats;
            for (j, &c) in cols.iter().enumerate() {
                out[i * k + j] = self.matrix[base + c];
            }
        }
    }
}

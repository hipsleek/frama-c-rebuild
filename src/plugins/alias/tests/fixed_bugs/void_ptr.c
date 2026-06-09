struct hash_entry {
  void *data;
};

struct hash_table {
  struct hash_entry bucket;
};

struct __anonunion_fts_cycle_29 {
  struct hash_table *ht;
} fts_read_sp;

struct _ftsent {
  struct _ftsent *fts_link;
} *fts_read_sp_0;

struct Active_dir {
  struct _ftsent *fts_ent;
};

struct Active_dir enter_dir_ad;
void *enter_dir_entry = &enter_dir_ad;

struct Active_dir *hash_find_entry(struct hash_table *table___0) {
  struct hash_entry *bucket = &table___0->bucket;
  return bucket->data;
}

void enter_dir(struct __anonunion_fts_cycle_29 *fts, struct _ftsent *ent) {
  enter_dir_ad.fts_ent = ent;
  void *data = hash_find_entry(fts->ht);
  data = enter_dir_entry;
}

int main() {
  enter_dir(&fts_read_sp, fts_read_sp_0);
  (void)fts_read_sp_0->fts_link;
}

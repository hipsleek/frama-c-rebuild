struct {
  char x;
} typedef strtol_error;

struct hash_entry {
  struct hash_entry *next;
} transfer_entries_new_bucket, transfer_entries_cursor, hash_rehash_cursor;

struct hash_table {
  struct hash_entry bucket;
  struct hash_entry free_entry_list;
};

_Bool next_prime_tmp;

struct hash_table *hash_free_table___0;
enum RCH_status { RC_ok } * rpl_fdopendir() { return 0; }
_Bool is_basic() { return 0; }
unsigned strnlen1() { return 0; }
void mbuiter_multi_next() {
  int tmp___1 = is_basic();
  if (tmp___1) tmp___1 = strnlen1();
}
_Bool is_zero_or_power_of_two() { return 0; }
void cycle_check() { is_zero_or_power_of_two(); }
int close_stream() { return 0; }
char *quotearg_colon();
void close_stdout() {
  close_stream();
  quotearg_colon();
}
int set_cloexec_flag() { return 0;}
strtol_error xstrtoul() { return (strtol_error) { .x = 0 }; }
void *xmalloc() { return 0; }
void *xmemdup() { return 0;}
void xstrdup() { (void)*(char*)xmemdup(); }
int *fts_open(int compar(int const **, int const **));
int xfts_open(int compar(int const **, int const **)) { fts_open(compar); return 0; }
_Bool cycle_warning_required() { return 0; }
char umaxtostr() { return 0; }
char *E_invalid_user = "", *change_file_owner_tmp___11;
char *E_invalid_group = "";
char *E_bad_spec = "";
char parse_with_separator() {
  xmemdup();
  xstrtoul();
  umaxtostr();
  xstrdup();
  xstrtoul();
  xstrdup();
  return 0;
}
void parse_user_spec_error_msg() {
  if (xmalloc())
    parse_with_separator();
}
char *quoting_style_args[] = {"", "", "", "", "", "", "", ""};
char quotearg_char() { return 0; }
char *quotearg_colon() { quotearg_char(); return 0; }
char quote_n() { return 0; }
char *quote() {  rpl_fdopendir(); return 0; }
int openat_safer() {return 0;}
unsigned open_safer_tmp___25;
int open_safer() {
  if (open_safer_tmp___25)
    while (1) {
      is_basic();
      strnlen1();
      int tmp___1 = is_basic();
      if (tmp___1 || strnlen1()) break;
    }
  mbuiter_multi_next();
  return 0;
}
_Bool i_ring_empty() { return 0; }
_Bool is_prime() { return 0; }
int next_prime() {
  while (1)
    next_prime_tmp = is_prime();
  return 0;
}
void compute_bucket_size() { int x = next_prime(); if(x) return; }
int hash_initialize(unsigned hasher(void const *, unsigned)) {
  compute_bucket_size();
  return 0;
}
void hash_free() { (void)&hash_free_table___0->bucket; }
struct hash_table free_entry_table___0;
void free_entry(struct hash_entry *entry) {
  entry = &free_entry_table___0.free_entry_list;
}
void *hash_find_entry_next;
void hash_find_entry() { free_entry(hash_find_entry_next); }
_Bool transfer_entries() {
  while (transfer_entries_new_bucket.next)
    free_entry(&transfer_entries_cursor);
  return 0;
}
_Bool(hash_rehash)() {
  while (transfer_entries_new_bucket.next)
    free_entry(&hash_rehash_cursor);
  while (transfer_entries_new_bucket.next)
    free_entry(&transfer_entries_cursor);
  transfer_entries();
  return 0;
}
void(hash_insert)() {
  hash_find_entry();
  hash_rehash();
}
int fts_alloc() { return 0; }
void fts_lfree() {
  int p;
  (void)&p;
}
int fts_maxarglen() { return 0; }
_Bool fts_palloc() { return 0; }
int fts_sort() { return 0; }
short fts_stat() { return 0; }
_Bool AD_compare() { return 0; }
unsigned AD_hash(void const *x, unsigned y) { return y;}
_Bool setup_dir() { hash_initialize(AD_hash); return 0; }
void free_dir() { hash_free(); }
_Bool fd_ring_clear_tmp___0;
void fd_ring_clear() {
  while (1) {
    fd_ring_clear_tmp___0 = i_ring_empty();
    if (fd_ring_clear_tmp___0)
      goto while_break;
  }
while_break:;
}
int diropen() {
  openat_safer();
  open_safer();
  set_cloexec_flag();
  return 0;
}
short fts_open_p_1;
int *fts_open(int compar(int const **, int const **)) {
  fts_maxarglen();
  fts_palloc();
  fts_alloc();
  while (1) {
    if (!0) {
      goto while_break;
    }
    fts_alloc();
    {
      {
        fts_stat();
      }
      fts_open_p_1 = fts_stat();
    }
  }
while_break:
  fts_sort();
  setup_dir();
  diropen();
  fts_lfree();
  return (void*)0;
}
int(fts_close)() {
  fd_ring_clear();
  hash_free();
  free_dir();
  return 0;
}
char *Version = "";
int chownat() { return 0; }
int lchownat() { return 0; }
void ignore_ptr() {}
void describe_change() { (void)*(char*)xmalloc(); }
enum RCH_status restricted_chown() { return 0; }
void change_file_owner() {
  char __trans_tmp_2;
  int chopt;
  (void)&chopt;
  (void)&chopt;
  (void)&chopt;
  (void)&chopt;
  ignore_ptr();
  (void)&chopt;
  rpl_fdopendir();
  (void)&__trans_tmp_2;
  quote();
  change_file_owner_tmp___11 = quote();
  cycle_warning_required();
  rpl_fdopendir();
  quote_n();
  quote_n();
  lchownat();
  restricted_chown();
  chownat();
  describe_change();
  xfts_open(0);
  fts_close();
}

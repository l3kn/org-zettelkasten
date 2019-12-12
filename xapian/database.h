#include <xapian.h>

using namespace std;

class Database {
  Xapian::WritableDatabase db;
  Xapian::QueryParser queryparser;
  Xapian::TermGenerator indexer;

public:
  Database(const string &dbpath);
  Xapian::MSet run_query(const string &querystring,
                         Xapian::doccount offset = 0,
                         Xapian::doccount pagesize = 30);

  Xapian::Document index_file(const string &path, const string &project);
  map<string, int64_t> get_files();

  void add_document(const string &path, const string &project);
  void update_document(int64_t id, const string &path, const string &project);
  void update_document_path(int64_t id, const string &path, const string &project);
  void delete_document(int64_t id);
};

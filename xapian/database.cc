#include <iostream>
#include <fstream>
#include <sstream>

#include "database.h"

using namespace std;

Database::Database(const string &dbpath) {
  db = Xapian::WritableDatabase(dbpath, Xapian::DB_CREATE_OR_OPEN);
  queryparser = Xapian::QueryParser();

  queryparser.set_stemmer(Xapian::Stem("en"));
  queryparser.set_stemming_strategy(queryparser.STEM_SOME);
  queryparser.add_prefix("link", "LN");
  queryparser.add_prefix("project", "PR");

  indexer = Xapian::TermGenerator();
  indexer.set_stemmer(Xapian::Stem("en"));
}

Xapian::MSet Database::run_query(const string &querystring,
                                 Xapian::doccount offset,
                                 Xapian::doccount pagesize) {
  Xapian::Query query = queryparser.parse_query(querystring);
  Xapian::Enquire enquire(db);
  enquire.set_query(query);
  return enquire.get_mset(offset, pagesize);
}

int match_keyword(const string& line, const string& prefix) {
  if (line.length() < prefix.length()) {
    return -1;
  }

  if (line.compare(0, prefix.length(), prefix) == 0) {
    int start = prefix.length();
    // Trim leading whitespace
    while (start <= line.length() && line[start] == ' ') {
      start += 1;
    }

    return start;
  }

  return -1;
}


Xapian::Document Database::index_file(const string &path, const string &project) {
  ifstream infile(path.c_str());
  string line;
  int match;

  Xapian::Document doc;
  indexer.set_document(doc);
  doc.add_value(0, project);
  doc.set_data(path);

  while (getline(infile, line)) {
    match = match_keyword(line, "#+TITLE:");
    if (match == -1) {
      match = match_keyword(line, "#+title:");
    }

    if (match >= 0) {
      string title(line.substr(match));
      indexer.index_text(title, 0, "S");
      indexer.index_text(title, 10);
      doc.add_value(1, title);
      continue;
    }

    match = match_keyword(line, "#+KEYWORDS:");
    if (match == -1) {
      match = match_keyword(line, "#+keywords:");
    }

    if (match >= 0) {
      stringstream ss(line.substr(match));
      string keyword;

      while (getline(ss, keyword, ' ')) {
        if (keyword.length() > 0) {
          indexer.index_text(keyword, 0, "K");
        }
      }
      continue;
    }

    indexer.index_text(line, 1);
  }

  indexer.index_text(project, 0, "PR");

  return doc;
}

map<string, int64_t> Database::get_files() {
  map<string, int64_t> db_files;

  for (Xapian::PostingIterator it = db.postlist_begin("");
	     it != db.postlist_end(""); ++it) {
	  int64_t id = *it;
	  Xapian::Document doc = db.get_document(id);
	  string path = doc.get_data();

	  db_files[doc.get_data()] = id;
	}

  return db_files;
}

void Database::add_document(const string &path, const string &project) {
  Xapian::Document doc = index_file(path, project);
  db.add_document(doc);
}

void Database::update_document(int64_t id, const string &path, const string &project) {
  Xapian::Document doc = index_file(path, project);
  db.replace_document(id, doc);
}

void Database::update_document_path(int64_t id, const string &path, const string &project) {
  Xapian::Document doc = db.get_document(id);
  doc.set_data(path);
  // This will replace the existing value
  doc.add_value(0, project);
  db.replace_document(id, doc);
}

void Database::delete_document(int64_t id) {
  db.delete_document(id);
}

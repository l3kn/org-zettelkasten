#include <xapian.h>

#include <cstdio>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <cstring>
#include "database.h"

using namespace std;

int main(int argc, char** argv) {
  if (argc < 3) {
    cerr << "Usage: " << argv[0] << " <command> <arguments>" << endl;
    cerr << "Commands:" << endl;
    cerr << "  search <db-path> <query-term>..." << endl;
    cerr << "  index <db-path>" << endl;
    cerr << "  index-file <db-path> <filename> <project>" << endl;
    cerr << "  delete-entry <db-path> <filename>" << endl;
    cerr << "  rename-entry <db-path> <old-filename> <new-filename> <new-project>" << endl;
    return 1;
  }

  const char* command = argv[1];

  if (strcmp(command, "search") == 0) {
    if (argc < 4) {
        cerr << "Usage:  " << argv[0] << " search <db-path> <query-term>..." << endl;
    }

    Database db(argv[2]);

    string querystring;
    for (argv += 2; *argv; ++argv) {
      if (!querystring.empty()) {
        querystring += ' ';
      }
      querystring += *argv;
    }

    Xapian::MSet mset = db.run_query(querystring);

    for (Xapian::MSetIterator m = mset.begin(); m != mset.end(); ++m) {
      Xapian::docid did = *m;
      const string & data = m.get_document().get_data();
      cout << data;
      cout << "\t";
      cout << m.get_document().get_value(0);
      cout << '\t';
      cout << m.get_document().get_value(1);
      cout << '\t';
      cout << m.get_percent();
      cout << endl;
    }
  } else if (strcmp(command, "index") == 0) {
    if (argc != 3) {
        cerr << "Usage:  " << argv[0] << " index <db-path>" << endl;
    }

    Database db(argv[2]);

    string line;
    map<string, string> fs_files;

    while (getline(cin, line)) {
      stringstream ss(line.c_str());

      string project;
      string path;

      if (!getline(ss, path, ' ')) {
        cerr << "Format: \"file-path file-project\"" << endl;
        return 1;
      }

      if (!getline(ss, project)) {
        cerr << "Format: \"file-path file-project\"" << endl;
        return 1;
      }

      fs_files.insert(pair<string, string>(path, project));
    }

    map<string, int64_t> db_files = db.get_files();

    map<string, string>::iterator fs_i = fs_files.begin();
    map<string, int64_t>::iterator db_i = db_files.begin();

    int added = 0;
    int deleted = 0;
    int updated = 0;

    for (;;) {
      if (fs_i == fs_files.end()) {
        for (; db_i != db_files.end(); db_i++) {
          db.delete_document((*db_i).second);
          deleted++;
        }
        break;
      } else if (db_i == db_files.end()) {
        for (; fs_i != fs_files.end(); fs_i++) {
          db.add_document((*fs_i).first, (*fs_i).second);
          added++;
        }
        break;
      } else if ((*fs_i).first == (*db_i).first) {
        db.update_document((*db_i).second, (*fs_i).first, (*fs_i).second);
        updated++;

        fs_i++;
        db_i++;
      } else if ((*fs_i).first < (*db_i).first) {
        db.add_document((*fs_i).first, (*fs_i).second);
        added++;

        fs_i++;
      } else {
        db.delete_document((*db_i).second);
        deleted++;

        db_i++;
      }
    }

    cout << "Added: " << added << " ";
    cout << "Updated: " << updated << " ";
    cout << "Deleted: " << deleted << endl;
  } else if (strcmp(command, "index-file") == 0) {
    if (argc != 5) {
      cerr << "Usage:  " << argv[0] << " ";
      cerr << "index-file <db-path> <filename> <project>" << endl;
    }

    Database db(argv[2]);
    const char* path = argv[3];
    const char* project = argv[4];

    map<string, int64_t> db_files = db.get_files();
    auto match = db_files.find(path);

    if (match != db_files.end()) {
      int64_t id = match->second;
      db.update_document(id, path, project);
      cout << "Updated document" << endl;
    } else {
      db.add_document(path, project);
      cout << "Added document" << endl;
    }
  } else if (strcmp(command, "delete-entry") == 0) {
    if (argc != 4) {
      cerr << "Usage:  " << argv[0] << " delete-entry <db-path> <filename>" << endl;
    }

    Database db(argv[2]);
    const char* filepath = argv[3];

    map<string, int64_t> db_files = db.get_files();
    map<string, int64_t>::iterator it = db_files.find(filepath);

    if (it != db_files.end()) {
      int64_t doc_id = it->second;
      db.delete_document(doc_id);
      cout << "Delete document " << doc_id << " for " << filepath << endl;
    } else {
      cerr << "Entry " << filepath << " not found" << endl;
      return 1;
    }
  } else if (strcmp(command, "rename-entry") == 0) {
    if (argc != 6) {
      cerr << "Usage:  " << argv[0] << " ";
      cerr << "rename-entry <db-path> <old-filename> <new-filename> <new-project>" << endl;
    }

    Database db(argv[2]);
    const char* old_filepath = argv[3];
    const char* new_filepath = argv[4];
    const char* new_project = argv[5];

    map<string, int64_t> db_files = db.get_files();
    map<string, int64_t>::iterator it = db_files.find(old_filepath);

    if (it != db_files.end()) {
      int64_t doc_id = it->second;
      db.update_document_path(doc_id, new_filepath, new_project);

      cout << "Updating document " << doc_id << ": " << old_filepath;
      cout << " -> " << new_filepath << endl;
    } else {
      cerr << "Entry " << old_filepath << " not found" << endl;
      return 1;
    }
  } else {
    cerr << "Unknown command: " << command << endl;
    return 1;
  }
}

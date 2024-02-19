#include <zim/archive.h>
#include <zim/entry.h>
#include <zim/item.h>
#include <zim/search.h>
#include <iostream>
#include <map>
#include <sstream>
#include "WolframLibrary.h"
#include "WolframNumericArrayLibrary.h"

using namespace std;
using namespace zim;

#define ZIM_LINK_VERSION 5

typedef MNumericArray ByteArray;

/*----------------------------------------------------------------------------*/

WolframLibraryData ld = nullptr;
WolframNumericArrayLibrary_Functions nas = nullptr;

ByteArray stringToByteArray(const string& str) {

  auto num_bytes = str.length();
  mint dims[1]; dims[0] = num_bytes;
  ByteArray ba;
  nas->MNumericArray_new(MNumericArray_Type_UBit8, 1, dims, &ba);
  void* ptr = nas->MNumericArray_getData(ba);
  memcpy(ptr, str.c_str(), num_bytes);

  return ba;
}

/*----------------------------------------------------------------------------*/

mint byteArrayLen(ByteArray ba)  { return nas->MNumericArray_getFlattenedLength(ba); };
char* byteArrayData(ByteArray ba) { return reinterpret_cast<char*>(nas->MNumericArray_getData(ba)); };

/*----------------------------------------------------------------------------*/

class NZListIter {
public:
    char* cur; char* end; int len;
    explicit NZListIter(char* cur, char* end) : cur(cur), end(end), len(cur < end ? strlen(cur) : 0) { }
    string operator*() const { return string(cur, len); }
    NZListIter& operator++() { if (cur < end) { cur += len + 1; len = strlen(cur); } return *this; }
    bool operator==(const NZListIter& other) const { return cur == other.cur; }
//    bool operator!=(const NZListIter& other) const { return !(*this == other); }
};

class NZListRange {
public:
    char* a; char* b;
    NZListRange(ByteArray ba) {
      a = byteArrayData(ba);
      b = a + byteArrayLen(ba);
    }

    NZListIter begin() const { return NZListIter(a, b); }
    NZListIter end()   const { return NZListIter(b, b); }
};

/*----------------------------------------------------------------------------*/

map<mint, Archive*> archive_map;

DLLEXPORT void archive_manage(WolframLibraryData, mbool mode, mint id) {
  if (mode == 1) {
    cout << "unregistering " << id << endl;
    if (archive_map.count(id) > 0) {
      delete archive_map[id];
      archive_map.erase(id);
    }
  } else {
    cout << "registering " << id << endl;
    archive_map[id] = nullptr;
  }
}

Archive* toArchive(unsigned int index) {
  auto a = archive_map.at(index);
  if (a == nullptr) throw invalid_argument("bad archive index");
  return a;
}

/*----------------------------------------------------------------------------*/

EXTERN_C DLLEXPORT mint WolframLibrary_getVersion(){
  cout << "getVersion" << endl;
  return WolframLibraryVersion;
}

EXTERN_C DLLEXPORT int WolframLibrary_initialize( WolframLibraryData libData) {
  ld = libData;
  nas = libData->numericarrayLibraryFunctions;
  cout << "registering LEM" << endl;
  ld->registerLibraryExpressionManager("ZimArchive", archive_manage);
  return 0;
}

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize( WolframLibraryData libData) {
  for (auto archive : archive_map) {
    auto id = archive.first;
    cout << "deleting archive #" << id << endl;
    delete archive.second;
    ld->releaseManagedLibraryExpression("ZimArchive", id);
  };
  cout << "unregistering LEM";
  ld->unregisterLibraryExpressionManager("ZimArchive");
  return;
}

#define ExportedFunction(name) EXTERN_C DLLEXPORT int name(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
#define Try { try
#define Catch catch (const exception& e) { cerr << e.what() << endl; return LIBRARY_FUNCTION_ERROR; } return LIBRARY_NO_ERROR; }

#define GetInt(i)       MArgument_getInteger(Args[i])
#define GetString(i)    MArgument_getUTF8String(Args[i])
#define GetByteArray(i) MArgument_getMNumericArray(Args[i])
#define GetArchive(i)   toArchive(MArgument_getInteger(Args[i]));

#define ReturnVoid()       return LIBRARY_NO_ERROR;
#define ReturnInt(v)       { MArgument_setInteger(Res, v); return LIBRARY_NO_ERROR; }
#define ReturnByteArray(v) { MArgument_setMNumericArray(Res, v); return LIBRARY_NO_ERROR; }
#define ReturnString(v)    { auto ba = stringToByteArray(v); ReturnByteArray(ba); };
#define ReturnStream(v)    ReturnString(v.str());

/*----------------------------------------------------------------------------*/

ExportedFunction(zimLoadedArchiveCount) {
  ReturnInt(archive_map.size());
}

/*----------------------------------------------------------------------------*/

ExportedFunction(zimGetLinkVersion) {
  ReturnInt(ZIM_LINK_VERSION);
}

/*----------------------------------------------------------------------------*/

ExportedFunction(zimOpenArchive) {

  auto id   = GetInt(0);
  auto path = GetString(1);

  auto archive = new Archive(path);
  archive_map[id] = archive;

  ReturnVoid()
}

/*----------------------------------------------------------------------------*/

ExportedFunction(zimArchiveEntryRedirects) Try {

  auto archive = GetArchive(0);

  stringstream stream;
  for (auto entry: archive->iterByTitle()) {
    if (!entry.isRedirect()) continue;
    auto redir = entry.getRedirectEntry();
    stream << entry.getTitle() << '\0' << redir.getTitle() << '\0';
  }
  ReturnStream(stream);

} Catch

/*----------------------------------------------------------------------------*/

ExportedFunction(zimArchiveEntryData) Try {

  auto archive  = GetArchive(0);
  int limit     = GetInt(1);
  uint8_t mask  = GetInt(2);

  stringstream stream;
  auto i = 0;
  for (auto entry: archive->iterByPath()) {
    if ((entry.isRedirect() ? 1 : 2) & mask) continue;
    stream << entry.getPath() << '\0' << entry.getTitle() << '\0';
    if (limit && ++i >= limit) break;
  }
  ReturnStream(stream);

} Catch

/*----------------------------------------------------------------------------*/

ExportedFunction(zimEntryContentsByTitle) Try {

  auto archive = GetArchive(0);
  auto title   = GetString(1);
  auto item = archive->getEntryByTitle(title).getItem(true);
  auto blob = item.getData();
  ReturnString(string(blob));

} Catch


/*----------------------------------------------------------------------------*/

ExportedFunction(zimEntryContentsByTitleList) Try {

  auto archive = GetArchive(0);
  auto titles = GetByteArray(1);

  stringstream stream;
  for (auto title : NZListRange(titles)) {
    auto item = archive->getEntryByTitle(title).getItem(true);
    auto blob = item.getData();
    stream << string(blob) << '\0';
  }
  ReturnStream(stream);

} Catch

/*----------------------------------------------------------------------------*/

ExportedFunction(zimSearch) Try {

  auto archive = GetArchive(0);
  auto qstr    = GetString(1);
  auto limit   = GetInt(2);

  auto searcher = Searcher(*archive);
  auto query = Query(qstr);
  auto search = searcher.search(query);

  auto results = search.getResults(0, limit);

  stringstream stream;
  for (auto entry: results) stream << entry.getTitle() << '\0';
  ReturnStream(stream);

} Catch

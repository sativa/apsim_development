//---------------------------------------------------------------------------
#include <stdlib.h>
#include <dir.h>

#include <iostream.h>
#include <fstream.h>
#include <string>
#include <vector>
#include "../io_functions.h"
#include "TestIoFunctions.h"

#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

//   for (char *ch = buf; *ch; ch++) if (*ch=='\\') *ch = '/';

void TestExpandFileName (void)
   {
   char buf[MAXPATH];
   getcwd(buf, MAXPATH);
   strcat(buf, "\\test.dat");
   std::string s = ExpandFileName("test.dat");
   BOOST_CHECK(s == buf);

   std::string t = ExpandFileName("blah/../test.dat");
   BOOST_CHECK(t == buf);
   }

void TestFileExists(void)
   {
   unlink("test.dat");
   BOOST_CHECK (FileExists("test.dat") == 0);
   ofstream out("test.dat");
   out.close();
   BOOST_CHECK (FileExists("test.dat") != 0);
   unlink("test.dat");
   BOOST_CHECK (FileExists("test.dat") == 0);
   }

void TestDirectoryExists(void)
   {
   rmdir("testdir");
   BOOST_CHECK (DirectoryExists("testdir") == 0);
   mkdir("testdir");
   BOOST_CHECK (DirectoryExists("testdir") != 0);
   rmdir("testdir");
   BOOST_CHECK (DirectoryExists("testdir") == 0);
   }

// TODO:
//getDirectoryListing
//locateFile
//getRecursiveDirectoryListing
//copyFiles
//copyDirectories
//copyFilesPreserveDirectories
//deleteFilesOrDirectories
//getYoungestFile
//renameOnCollision

void TestIoFunctions(void)
   {
   TestExpandFileName();
   TestFileExists();
   TestDirectoryExists();
   }

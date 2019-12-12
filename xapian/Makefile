CC=gcc
CXX=c++
RM=rm -f
CPPFLAGS=-g -I/usr/local/include
LDFLAGS=-L/usr/local/lib
LDLIBS=-lxapian

SRCS=org-xapian.cc
OBJS=$(subst .cc,.o,$(SRCS))

all: org-xapian

org-xapian: database.o org-xapian.o
	$(CXX) $(LDFLAGS) -o org-xapian database.o org-xapian.o $(LDLIBS)

org-xapian: org-xapian.cc database.h

database.o: database.h database.cc

clean:
	$(RM) $(OBJS)

distclean: clean
	$(RM) org-xapian

# Makefile for 
# Copyright (C) 2008 Randall Gray May
# $Header$
# $Log$

#GAMBITDIR=/usr/local/Gambit/current
GAMBITDIR=/usr
CC = gcc

CFLAGS = -ggdb -O2 -I$(GAMBITDIR)/include
#CFLAGS = -ggdb -O0

#CFLAGS += -pg #profile the code
#CFLAGS += -g  # line-by-line
#CFLAGS += -fprofile-arcs -ftest-coverage  # test-coverage

#CFLAGS += -a # and basic blocks
#CFLAGS += -ax # and basic blocks with extended format

PRELUDE = -prelude '(load "/usr/lib/slib/gambit.init") (define (load x) \#t)'
FLAGS = $(PRELUDE) -report -warnings -debug -track-scheme
#FLAGS = $(PRELUDE) -report -warnings -track-scheme

# ************  NOTE WELL: simulation.o must be last!  ************
OBJS = rk4.o integrate.o maths.o models.o sort.o helpers.o config.o units.o kernel.o simulation.o 

CS = $(OBJS:.o=.c)
SCMS = $(OBJS:.o=.scm)

all: 
	make restore || true
	make simulation
	make hide || true


restore:
	touch .saved/halt
	mv .saved/* .
	rm -f halt

hide:
	touch saving-intermediates
	mv saving-intermediates *.o[0-9] *.gcda *.gcno $(CS) simulation_.c* .saved || true
	rm .saved/saving-intermediates

simulation_.c: $(CS)
	gsc $(FLAGS) -link $(CS) 

%.c:	%.scm
	gsc $(FLAGS) -c $<

simulation:	simulation_.c
	gcc $(CFLAGS) -o simulation -I$(GAMBITDIR)/include $(CS) simulation_.c -L$(GAMBITDIR)/lib -lgambc -lm -ldl -lutil
	rm -f *.gcda *.gcno

clean:
	rm -f *.o[0-9] *.gcda *.gcno $(CS) simulation_.c simulation .saved/*


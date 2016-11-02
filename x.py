#! /usr/bin/env python3
#
# druwid is machine learning tool for adverse drug discovery
#
# It relies on the Aleph ILP learner, written and maintained by Ashwin Srinivasan\
#
# Authos: Vitor Santos Costa, David Page
# Bugs are from Vitor Santos Costa
#

import matplotlib
import matplotlib.image as mpimg
#matplotlib.use('Agg')

import argparse
import csv
import heapq
import logging
import networkx as nx
import os
import numpy as np
import pandas as pd
import sys
import threading
import time
import yap

graphics_ability = False

if graphics_ability:
    import PIL
    def display_pdf(id):
        im = Image.open(self.shown_clause[id])
        im.show()


logging.basicConfig(level=logging.DEBUG,
                    format='[%(levelname)s] (%(threadName)-10s) %(message)s',
                    )


from collections import namedtuple
from enum import IntEnum
from queue import Queue
from dru.druplot import plotClause
from dru.shell import alephShell

# class Console(InteractiveConsole):

#  def __init__(*args): InteractiveConsole.__init__(*args)

compile = namedtuple('consult', 'FileName')
ensure_loaded = namedtuple('ensure_loaded', 'FileName')
loadFile = namedtuple('load_file', 'FileName Opts')
add_example = namedtuple('add_example', 'polarity case id b e')
set = namedtuple('set', 'key val')
setting = namedtuple('setting', ' key')
clsrc = namedtuple('clsrc', ' key ids')
clgraph = namedtuple('clgraph', ' key')
clhist = namedtuple('clhist', ' key first pos')
clause_info = namedtuple('clause_info', ' key Text Symbs H1Pos H2Pos CH1Pos CH2Pos  ')
learn = namedtuple('learn', 'example')
learn_in_thread = namedtuple('learn_in_thread', 'example')
#learner = namedtuple('learn', 'class')
# assert = namedtuple('assert', 'fact')
load_ptable = namedtuple('load_ptable', 'File')
load_files = namedtuple('load_files', 'File Opts')


# prolog engine
class y:
  E = None

  def run(g):
    y.E.goal(g)

  def f(g):
    y.E.fun(g)


# Schema information on Marshfiel mode table ( 2016 data )
#
# TBD: make it match/ genrate a mode declaration
#

# column headers, StudyId refers to the study participant
#
class DiagFields:
  StudyID = 0
  DX_CODE= 1
  AGE= 2
  FACILITY_NUM= 3
  PROV_ID= 4
  DX_DESC= 5
  DX_TYPE_ID= 6
  DX_TYPE_DESC= 7
  DX_SUB_TYPE_ID= 8
  DX_SUB_TYPE_DESC= 9
  DX_CODE_CATEGORY= 10
  DX_CODE_CATEGORY_DESC= 11
  DX_CODE_SUBCATEGORY= 12
  DX_CODE_SUBCATEGORY_DESC= 13
  DATA_SOURCE= 14

#
# operations to fetch data from meds
#
class DiagOps(DiagFields):
  ''' Selects age, id, and one descriptor: we chose to use DX_DESC so that people
  can understand the rules easily. '''

  def import_row( self ):
    return ( DiagFields.StudyID, DiagFields.AGE, DiagFields.DX_DESC )

  def pred(self):
    return yap.YAPPrologPredicate( self.name, 3 )

  def __init__(self, name, ids):
    self.name = name
    self.ids = ids

# column headers, StudyId refers to the study participant
#
class MedFields( IntEnum ):
  StudyID = 0
  AGE =  1
  GCN_SEQ_NUM= 2
  DRUG_NAME= 3
  GENERIC_NAME= 4
  DOSAGE= 5
  FREQUENCY= 6
  ACTION_ATTRIBUTE_DESC= 7
  ACTION_VALUE_DESC = 8
  ACTION_IN_PLAN_CODE= 9
  THERAPEUTIC_GENERIC_ID= 10
  THERAPEUTIC_GENERIC_DESC= 11
  THERAPEUTIC_SPECIFIC_ID= 12
  THERAPEUTIC_SPECIFIC_DESC= 13
  DRUG_SOURCE= 14
  DATA_SOURCE = 15

  #
  # operations to fetch data from meds
  #
class MedOps:
  ''' Operations as designed for the Marshfield meds table'''

  arity = 3

  def import_row( self ):
    return ( MedFields.StudyID, MedFields.AGE, MedFields.DRUG_NAME )

  def pred( self ):
    return yap.YAPPredicate( self.name, 3 )

  def __init__(self, name, ids):
    self.name = name
    self.ids = ids

class PrologTable:
  '''Access tables in Prolog format'''

  def query( self ):
    args = [ 0 for x in range(self.arity) ]
    return self.pname._make( args )

  def __init__(self, p, name):
    self.p = p
    self.name = name
    self.arity = p.arity()
    ArgNames = [ "A" + str(x+1) for x in range(self.arity) ]
    self.pname = namedtuple(self.name, ArgNames)

  def __iter__(self):
    goal = self.pname._make(  )
    return PrologTableIter(self, e, goal)

class PrologTableIter:

  def __init__(self, e, goal):
    try:
      self.e = e
      self.q = e.YAPQuery(goal)
    except:
      print('Error')

  def __iter__(self):
    # Iterators are iterables too.
    # Adding this functions to make them so.
    return self

  def next(self):
    if self.q.next():
      return goal
    else:
      self.q.close()
      self.q = None
      raise StopIteration()

class DBStore:
  '''store operations: csv to pl, and so on'''

  def filter ( self, row ):
    id = int(row[self.StudyID])

    if id in self.ids:
      ex1 = self.ids[ id ]
      ex2 = self.ids[ -id ]

      age = int(float(row[self.AGE])*1000)
      if ex2[1] <= age and age <= ex2[2]:
        id = -id
      elif ex1[1] > age or age > ex1[2]:
        return None
      desc = row[self.DESC]
      return id, age, desc

  def __init__(self, File, dbi, ids ):
    self.ids = ids
    OFile = "data/" + dbi.name + '.yap'
    if os.path.isfile(OFile) :
      print("loading db from "+OFile)
      y.run( load_files( OFile , []) )
      return
    with open(File) as csvfile:
      print("Converting db from "+File+ " to "+OFile)
      with open( OFile, "w") as out:
        csvfile.seek(0)
        reader = csv.reader(csvfile, delimiter = '|', quoting = csv.QUOTE_MINIMAL )
        ( self.StudyID, self.AGE, self.DESC ) = dbi.import_row()
        P = dbi.pred()
        reader.__next__()
        for row in reader:
          tuple = self.filter( row )
          if tuple:
            out.write( dbi.name + "( " + str(tuple[0]) +" , " + str(tuple[1])+ ", \'" +  str(tuple[2]) + "\').\n" )
      print("loading db from "+OFile)
      y.E.reSet()
      y.run( load_ptable( OFile ) )

  def save_table(self, File, name):
    p = self.YAPPredicate(name, 3)
    with open(File,  'w', newline='') as csvfile:
      fieldnames = ['Id', 'Age', 'Attribute' ]
      writer = csv.writer(csvfile, delimiter='|', fieldnames=fieldnames)
      writer.writerows(PrologTable(p, name))

class Examples:
  ''' Support for the manipulation and processing of examples.

      So far, only loadng examples'''

  ids = {}

  def __init__(self, File):
    if File.lower().endswith(('.yap','.pl','.pro','.prolog')):
      E.run( add_prolog( File ) )
      return
    print("loading examples from "+File)
    with open(File) as csvfile:
      dialect = csv.Sniffer().sniff(csvfile.read(1024))
      dialect.delimiter = '|'
      dialect.quoting = csv.QUOTE_MINIMAL
      csvfile.seek(0)
      reader = csv.reader(csvfile, dialect)
      reader.__next__()
      for row in reader:
        ( cdb, pdb, id, b, e ) = row
        case = cdb  == "1" or cdb == 't' or cdb == '+'
        Type = pdb  == "1" or pdb == 't' or pdb == '+'
        if Type:
          id = int(id)
          ti = 1
        else:
          id = -int(id)
          ti = 0
        if case:
          ci = 1
        else:
          ci = 0
        b = int(float(b)*1000)
        e = int(float(e)*1000)
        y.run( add_example(ti, ci, id, b, e) )
        self.ids[id] = ( case, b, e )

cols = ['Id', 'Ref', 'Parent', 'TPP', 'TPN', 'TNN', ' CPP', 'CPN', 'CNN']
indx= ['Id']



class ClauseQueue:
  '''Auxiliary class that represents the list of visited clauses.'''

  ''' queue size '''
  size = 1024*256
  best = 8
  q = []
  count = 0

  def parentText(self, parent):
    [row] = self.DF.loc[self.DF.Id==parent].values.tolist( )
    return "Parent "+str(parent)+", cases " +repr(row[3:6])+", controls " +repr(row[6:9])

  def showQueue(self, n):
    L = heapq.nlargest(n, self.q)
    S = "[ *********************************************************************\nbest rules at " + repr(self.count) +" nodes:\n"
    S += "Node".rjust(6) + "Score".rjust(10) + "Parent".rjust(6) +" | " +"Matches on Cases".center(24) +" | " +"Matches on Controls".center(24) + '|\n'
    S += "".rjust(6) + "".rjust(10) + "".rjust(6) + " | " +"Generic".center(8) + "Both".center(8) + "Brand".center(8) + " | " +"Generic".center(8) + "Both".center(8) + "Brand".center(8) + '|\n'
    S += "".rjust(6) + "".rjust(10) + "".rjust(6) + " | " + "Only".center(8) + "".center(8) + "Only".center(8) + " | " +"Only".center(8) + "".center(8) + "Only".center(8) + '|\n'
    for cl in L:
      S += self.clauseToStringRow( cl )
    S += "\n[ ********************************************************************* ]\n\n"
    for cl in L:
      S += self.PrintClbyId( cl )
    return S


  def loadHists(self):
    hists = {}
    if self.ipcs[0]:
        hists["case_after_first"] = self.histpcs[0][0:self.ipcs[0]]
    if self.ipcs[1]:
        hists["case_after_last"] = self.histpcs[1][0:self.ipcs[1]]
    if self.ipcs[2]:
        hists["case_bef_first"] = self.histpcs[2][0:self.ipcs[2]]
    if self.ipcs[3]:
        hists["case_bef_last"] = self.histpcs[3][0:self.ipcs[3]]
    if self.ipcs[4]:
        hists["control_after_first"] = self.histpcs[4][0:self.ipcs[4]]
    if self.ipcs[5]:
        hists["control_after_last"] = self.histpcs[5][0:self.ipcs[5]]
    if self.ipcs[6]:
        hists["control_bef_first"] = self.histpcs[6][0:self.ipcs[6]]
    if self.ipcs[7]:
        hists["control_bef_last"] = self.histpcs[7][0:self.ipcs[7]]
    return hists

  def attendRequests(self):
    while not self.command_q.empty():
      msg = self.command_q.get()
      if msg[0] == "show_clause":
        row = msg[1]
        y.run( clsrc( row[1], self ) )
        parent = row[2]
        parentDesc = self.parentText(parent)
        self.hists = self.loadHists()
        print( hists)
        self.reply_q.put( ("show_clause", parentDesc ) )


  # this method implements PrintCl if YAP is running
  def printClWithThreads(self, row):
    try:
      id = row[0]
      # if id in self.shown_clause:
      #   im = Image.open(self.shown_clause[id])
      #    im.show()
      #  return
      #Prolog does the firat half
      self.queue.prolog_q.put( ( "show_clause" , row ) )
      ( x, parentDesc )= self.queue.reply_q.get()
      self.shown_clause[id] = plotClause(row[0],parentDesc, row[3:6], row[3:9], Text, (self.GraphV,self.d), self.hists)
    except Exception as e:
      print( 'trieref = ' + trieref )
      raise

  # this method implements PrintCl if YAP is not running
  def printClNoThreads(self, row):
    try:
      id = row[0]
      if graphics_ability and id in self.shown_clause:
        display_pdf( id )
        im = Image.open(self.shown_clause[id])
        im.show()
        return
      #Prolog does the real work
      y.run( clsrc( row[1], self ) )
      parent = row[2]
      self.hists = self.loadHists()
      parentDesc = self.parentText(parent)
      # and then sealib
      self.shown_clause[id] = plotClause(row[0],parentDesc, row[3:6], row[6:9], self.Text )
    except Exception as e:
      print( 'trieref = ' + trieref )
      raise

  def clauseToStringRow(self, id):
    try:
      [row] = self.DF.loc[self.DF.Id==id].values.tolist( )
      S = "" + repr(id).rjust(6) + "{:10.3f}".format(cl[0]) + repr(row[2]).rjust(6) +" | " + repr(row[3]).rjust(6)  + repr(row[4]).rjust(6) + repr(row[5]).rjust(6) + ' | '+ repr(row[6]).rjust(6) + repr(row[7]).rjust(6) +  repr(row[8]).rjust(6) + '|\n'
      return S
    except Exception as e:
      print( str(e) )
      raise

  def printClauseAsRow(self, id):
    print( self.clauseToStringRow( id ) )


  def printClbyId(self, id):
    try:
      [row] = self.DF.loc[self.DF.Id==id].values.tolist( )
      self.printClause( row )
    except Exception as e:
      print( str(e) )
      raise

  def printClbyTrieRef(self, trieref):
    try:
      [row] = self.DF.loc[self.DF.Ref==trieref].values.tolist()
      self.printClause( row )
    except Exception as e:
      print( str(e) )
      raise

  def idFromTrieRef( self, trieref ):
    try:
      row = self.DF.loc[self.DF.Ref==trieref]
      return int(row.at['Id','Id' ])
    except Exception as e:
      print("node = "+str(trieref))
      print(self.DF)
      raise

  def add(self, parent, score, trieref, c):
    try:
      #import pdb
      #pdb.set_trace()
      self.count += 1
      k = [self.count,trieref,parent,c[0],c[1],c[2],c[3],c[4],c[5]]
      heapq.heappush(self.q, (score, self.count))
      self.DF = self.DF.append(pd.DataFrame([k],columns=cols,index=indx))
      if not self.command_q.empty():
        self.attendRequests()
    except Exception as e:
      print("new node = "+str(self.count))
      print("parent = "+str(parent))
      print(self.DF)
      raise

  def link(self, parent, trieref):
    try:
      row = self.DF.loc[self.DF.Ref==trieref]
      if not self.command_q.empty():
        self.attendRequests()
    except Exception as e:
      print("new node = "+str(trieref))
      print("parent = "+str(parent))
      print(self.DF)
      raise

  def pushHistogram( self, i, val):
    try:
      x = self.ipcs[i]
      self.histpcs[i][x]  = val
      self.ipcs[i] = x+1
    except Exception as e:
      print("i = "+str(i))
      print("x = "+str(x))
      print(self.DF)
      raise


  def initHistograms( self ):
    self.histpcs = ( [None]*2400, [None]*2400, [None]*2400, [None]*2400,
                     [None]*2400, [None]*2400, [None]*2400, [None]*2400)
    self.resetHistograms()

  def resetHistograms( self ):
    self.ipcs = [ 0, 0, 0, 0, 0, 0, 0, 0 ]

  def setClauseText( self, txt ):
    self.Text = txt

  def setClauseGraph( self, labels,edges ):
    G=nx.DiGraph()
    dict = {}
    for (i,l) in labels:
      G.add_node(i,label=i)
      dict[i] = l.strip()[0].lower()
    for (i,j) in edges:
      G.add_edge(i,j)
    self.GraphV = G
    self.d = dict
    return G

  def __repr__(self):
    l = heapq.nlargest(self.q, 10)
    for i in l:
      print( l )

  def __init__(self):
    self.command_q = Queue()
    self.reply_q = Queue()
    self.GraphV=nx.Graph()
    self.count = 0
    self.DF = pd.DataFrame([[0,88998993,0,4,3,2,1,160,400]],columns=cols, index=indx)
    self.shown_clause = {}

class LineSettings:
  '''Isolate interface with argparse '''



  opts = None

  def __init__(self):
    parser = argparse.ArgumentParser(description='''Search for ADRs using EHR data.
    The arguments are CSV files for the databases, with at least 3 fields:
    - an integer giving the patient id, called key
    - a float point giving  the patient\'s age in years, called age
    - a string describing the diagnosis, called data
dppb
    The case and control files are alos in CSV form, withe the following fields:
    Key,AgeBefStart,AgeStartEnd,AgeAfterStart,AGeAfterEnd

p    ''')
    parser.add_argument('--save-db', dest='save', default=None, help="save the processed DB in Prolog, CSV, pickle")
    parser.add_argument('--meds', dest='meds', default="data/meds.csv", help="CSV or Tab like with the medications database")
    parser.add_argument('--diags', dest='diags', default="data/diags.csv", help="CSV or Tab like with the medications database")
    parser.add_argument('--examples', dest='examples', default="data/exs.csv" , help="CSV or Tab like with the cases and controls")
    parser.add_argument('--labs', type=argparse.FileType('r'), default=None, help="unsupported for now")
    parser.add_argument('--min_examples', type=int, default=20, help="minimal number of examples to cover")
    parser.add_argument('--seed', type=int, default=0, help="examples to start search, 0 if tries to cinsider all")
    parser.add_argument('-f', default=" " , help="jupyter notebook")
    parser.add_argument('--interactive', type = bool, default=True, help="run as line mode, or run as closed script ()")
    self.opts = parser.parse_args()

  def map(self):
    return vars(self.opts)


class Aleph:

  e = None

  def add_db(self, p, t):
    queue.addClause(t)

  def set_options( self, xargs):

    if 'min-examples' in xargs:
      self.set('minpos', xargs[ 'min_examples' ] )
    if 'verbosity' in xargs:
      self.set('verbosity', xargs[ 'verbosity' ] )
    if 'search' in xargs:
      self.set('search', xargs[ 'search' ] )
    if 'nodes' in xargs:
      self.set('nodes', xargs[ 'nodes' ] )

  def set( self, parameter, value):
    '''Set an integer parameter, eg nodes, seeds or noise'''
    y.run(set(parameter, value))

  def setting( self, parameter):
    '''Return the Aleph setting for parameter p, or show all
       the current settings'''
    if parameter:
      value = yap.YAPVarTerm()
      y.run(setting(parameter, value))
    #  return value
    y.run( settings )

  def induce( self, index = 0):
    '''Learn clauses'''
    y.run( learn( index ) )

  def induceInThread( self, index = 0):
    '''Learn clauses as a separe thread'''
    if self.learning:
      print("Already learning" )
      return
    self.learning = True
    y.run( learn_in_thread( index ) )

  def  query_prolog( self, y, Query):
    y.run( Query )

  def rule( self, id ):
      self.queue.printClause( id )

  def histogram( self, Dict ):
    pass

  def induceInThread( self, index = 0 ):
    kw = {}
    kw["index"] = index
    t = threading.Thread(target=self.induceInThread, kwargs=kw)
    t.setDaemon = True
    self.queue.printClause = self.queue.printClWithThreads
    t.start()
    self.queue.printClause = self.queue.printClNoThreads

  def rules( self, count = 100 ):
    self.queue.showQueue()

  def golearn( self ):

    try:
      # import pdb
      # pdb.set_trace()
      self.learning = False
      alephShell( self ).cmdloop()
      q.close()

    except SyntaxError as err:
      print("Syntax Error error: {0}".format(err))
      print( sys.exc_info()[0] )
    except EOFError:
      return
    except RuntimeError as err:
      print("YAP Execution Error: {0}".format(err))
      print( sys.exc_info()[0] )
    except ValueError as err:
      print("Could not convert data to an integer: {0}.", format(rr))
      print( sys.exc_info()[0] )
    except NameError as err:
      print("Bad Name: {0}.", format(err))
      print( sys.exc_info()[0] )
    except Exception as err:
      print("Unexpected error:" + sys.exc_info() )
      print( sys.exc_info()[0] )


  def learn( self ):

    while True:
      self.golearn()

  def __init__(self, queue):
    ''' Initialize Aleph by loading the data-bases and the example'''

    if y.E == None:
      y.E = yap.YAPEngine()
    y.run( ensure_loaded( sys.druwid_root +'/druwid.yap' ) )
    y.E.reSet()
    x_args = LineSettings().map()
    exf = x_args['examples']
    exs = Examples(exf)
    di = x_args['diags']
    exmap = exs.ids
    diags = DBStore( di, DiagOps( "diags", exmap ), exmap )
    md = x_args['meds']
    meds = DBStore(md, MedOps( "meds", exmap ) , exmap )
    y.E.reSet()
    save_db = x_args['save']
    self.set_options( x_args )
    self.interactive = x_args['interactive']
    self.queue = queue
    self.learning = False
    self.queue.initHistograms( )
    self.queue.printClause = self.queue.printClNoThreads

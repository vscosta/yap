// -*- c++ -*-
//=============================================================================
// Copyright (C) 2011 by Denys Duchier
//
// This program is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation, either version 3 of the License, or (at your
// option) any later version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
// 
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//=============================================================================

#ifndef __GECODE_DISJUNCTOR_HH__
#define __GECODE_DISJUNCTOR_HH__

#include <gecode/kernel.hh>
#include <gecode/int.hh>
#include <gecode/set.hh>

namespace Gecode { namespace Disjunctor_ {

    using namespace generic_gecode;

    //=========================================================================
    // from a user perspective, a Disjunctor corresponds to a set of
    // speculative clauses:
    //
    // - when a non-committed clause fails, it is simply discarded
    // - when a single clause remains, it is committed
    //
    // from an implementation perspective, a Disjunctor is an object shared by
    // several clauses, that contains a counter keeping track of how many
    // clauses remain.  when the counter reaches 1, the remaining clause
    // becomes committed.
    //=========================================================================

    // following the example in MPG
    struct Disjunctor: public LocalHandle
    {
      Disjunctor();
      Disjunctor(Space& home);
      Disjunctor(const Disjunctor& d);
      int get() const;
      void incr();
      void decr();
      void dispose(Space& home);
    };

    //=========================================================================
    // a Clause encapsulates the speculative execution of a set of propagators.
    // Therefore, these propagators must be posted in a separate (sub)space.
    // However they normally want to constrain variables of the original home
    // space: for each such variable X, a variable Y local to the subspace must
    // be created and used instead, and a forwarder must be created from X to
    // Y. A Clause is then a Propagator that subscribes to the Xs.  Whenever a
    // X changes, the domains of the Xs are forwarded into the Ys and
    // constraint propagation is run in the subspace.  When a clause is
    // committed, after propagation, the domains of Ys are forwarded back into
    // the Xs.
    //=========================================================================

    // TODO: copy (difficulty is forwarder list)

    class SubSpace;

    class Clause: public Propagator {
      Disjunctor disj;
      SubSpace*const subhome;
    public:
      Clause(Space& home_, Disjunctor disj_);
      Clause(Space& home, bool share, Clause& c);
      virtual Propagator* copy(Space& home, bool share);
      virtual size_t dispose(Space& home);
      virtual PropCost cost(const Space&, const ModEventDelta&) const;
      virtual ExecStatus propagate(Space& home, const ModEventDelta&);
      // user API
      operator GenericSpace&();
      operator Space&();
      operator Home();
      GenericSpace* generic_space() const;
      Space* space() const;
      void forward(Space& home, IntVar  outer, IntVar  inner);
      void forward(Space& home, BoolVar outer, BoolVar inner);
      void forward(Space& home, SetVar  outer, SetVar  inner);
    protected:
      ExecStatus forward_in();
      ExecStatus forward_out(Space& outer_home);
      bool committed() const;
    };

    //=========================================================================
    // a SubSpace is the Space associated with a Clause.  It also contains the
    // forwarder linked-list because we want it to manage the memory for that
    // list.
    //=========================================================================

    struct BasicForwarder;
    using Int::IntView;
    using Int::BoolView;
    using Set::SetView;

    class SubSpace: public GenericSpace
    {
    private:
      Space* homeDuringCloning;
      // linked-list of pairs of a variable in the parent space and a
      // corresponding variable in the subspace
      BasicForwarder* forwarder;
      // auxiliary methods for adding an entry to the forwarder list
      void forward(IntView  outer, IntView  inner);
      void forward(BoolView outer, BoolView inner);
      void forward(SetView  outer, SetView  inner);
      // for forwarding domains into and out of the subhome
      ExecStatus forward_in();
      ExecStatus forward_out(Space&);
      // for cancelling subscriptions
      void cancel(Space& home, Clause&);
      // allow Clause to use this private API
      friend class Clause;
    public:
      SubSpace();
      SubSpace(bool share, SubSpace& s);
      virtual Space* copy(bool share);
      SubSpace* copy(Space& home, bool share);
    };
}}

namespace Gecode
{
  using Disjunctor_::Disjunctor;

  class Clause
  {
    generic_gecode::GenericSpace& _home;
    Disjunctor_::Clause* _clause;
  public:
    Clause(generic_gecode::GenericSpace& home, Disjunctor disj);
    operator generic_gecode::GenericSpace&();
    operator Space&();
    operator Home();
    generic_gecode::GenericSpace* generic_space();
    generic_gecode::GenericSpace* generic_parent();
    Space* space();
    void forward(IntVar  outer, IntVar  inner);
    void forward(BoolVar outer, BoolVar inner);
    void forward(SetVar  outer, SetVar  inner);
  };
}

#endif

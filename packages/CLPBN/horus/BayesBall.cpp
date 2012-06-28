#include <cstdlib>
#include <cassert>

#include <iostream>
#include <fstream>
#include <sstream>

#include "BayesBall.h"
#include "Util.h"


FactorGraph*
BayesBall::getMinimalFactorGraph (const VarIds& queryIds)
{
  assert (fg_.bayesianFactors());
  Scheduling scheduling;
  for (size_t i = 0; i < queryIds.size(); i++) {
    assert (dag_.getNode (queryIds[i]));
    BBNode* n = dag_.getNode (queryIds[i]);
    scheduling.push (ScheduleInfo (n, false, true));
  }

  while (!scheduling.empty()) {
    ScheduleInfo& sch = scheduling.front();
    BBNode* n = sch.node;
    n->setAsVisited();
    if (n->hasEvidence() == false && sch.visitedFromChild) {
      if (n->isMarkedOnTop() == false) {
        n->markOnTop();
        scheduleParents (n, scheduling);
      }
      if (n->isMarkedOnBottom() == false) {
        n->markOnBottom();
        scheduleChilds (n, scheduling);
      }
    }
    if (sch.visitedFromParent) {
      if (n->hasEvidence() && n->isMarkedOnTop() == false) {
        n->markOnTop();
        scheduleParents (n, scheduling);
      }
      if (n->hasEvidence() == false && n->isMarkedOnBottom() == false) {
        n->markOnBottom();
        scheduleChilds (n, scheduling);
      }
    }
    scheduling.pop();
  }

  FactorGraph* fg = new FactorGraph();
  constructGraph (fg);
  return fg;
}



void
BayesBall::constructGraph (FactorGraph* fg) const
{
  const FacNodes& facNodes = fg_.facNodes();
  for (size_t i = 0; i < facNodes.size(); i++) {
    const BBNode* n = dag_.getNode (
        facNodes[i]->factor().argument (0));
    if (n->isMarkedOnTop()) {
      fg->addFactor (facNodes[i]->factor());
    } else if (n->hasEvidence() && n->isVisited()) {
      VarIds varIds = { facNodes[i]->factor().argument (0) };
      Ranges ranges = { facNodes[i]->factor().range (0) };
      Params params (ranges[0], LogAware::noEvidence());
      params[n->getEvidence()] = LogAware::withEvidence();
      fg->addFactor (Factor (varIds, ranges, params));
    }
  }
  const VarNodes& varNodes = fg_.varNodes();
  for (size_t i = 0; i < varNodes.size(); i++) {
    if (varNodes[i]->hasEvidence()) {
      VarNode* vn = fg->getVarNode (varNodes[i]->varId());
      if (vn) {
        vn->setEvidence (varNodes[i]->getEvidence());
      }
    }
  }
}


#include "Solver.h"


void
Solver::printAllPosterioris (void)
{
  const VarNodes& vars = gm_->getVariableNodes();
  for (unsigned i = 0; i < vars.size(); i++) {
    printPosterioriOf (vars[i]->varId());
    cout << endl;
  }
}



void
Solver::printPosterioriOf (VarId vid)
{
  VarNode* var = gm_->getVariableNode (vid);
  const ParamSet& posterioriDist = getPosterioriOf (vid);
  const States& states = var->states();
  for (unsigned i = 0; i < states.size(); i++) {
    cout << "P(" << var->label() << "=" << states[i] << ") = " ;
    cout << setprecision (PRECISION) << posterioriDist[i];
    cout << endl;
  }
  cout << endl;
}



void
Solver::printJointDistributionOf (const VarIdSet& vids)
{
  VarNodes vars;
  VarIdSet vidsWithoutEvidence;
  for (unsigned i = 0; i < vids.size(); i++) {
    VarNode* var = gm_->getVariableNode (vids[i]);
    if (var->hasEvidence() == false) {
      vars.push_back (var);
      vidsWithoutEvidence.push_back (vids[i]);
    }
  }
  const ParamSet& jointDist = getJointDistributionOf (vidsWithoutEvidence);
  vector<string> jointStrings = Util::getJointStateStrings (vars);
  for (unsigned i = 0; i < jointDist.size(); i++) {
    cout << "P(" << jointStrings[i] << ") = " ;
    cout << setprecision (PRECISION) << jointDist[i];
    cout << endl;
  }
  cout << endl;
}


#include "Solver.h"
#include "Util.h"


void
Solver::printAllPosterioris (void)
{
  const VarNodes& vars = gm_->getVariableNodes();
  for (unsigned i = 0; i < vars.size(); i++) {
    printPosterioriOf (vars[i]->varId());
  }
}



void
Solver::printPosterioriOf (VarId vid)
{
  VarNode* var = gm_->getVariableNode (vid);
  const Params& posterioriDist = getPosterioriOf (vid);
  const States& states = var->states();
  for (unsigned i = 0; i < states.size(); i++) {
    cout << "P(" << var->label() << "=" << states[i] << ") = " ;
    cout << setprecision (Constants::PRECISION) << posterioriDist[i];
    cout << endl;
  }
  cout << endl;
}



void
Solver::printJointDistributionOf (const VarIds& vids)
{
  VarNodes vars;
  VarIds vidsWithoutEvidence;
  for (unsigned i = 0; i < vids.size(); i++) {
    VarNode* var = gm_->getVariableNode (vids[i]);
    if (var->hasEvidence() == false) {
      vars.push_back (var);
      vidsWithoutEvidence.push_back (vids[i]);
    }
  }
  const Params& jointDist = getJointDistributionOf (vidsWithoutEvidence);
  vector<string> jointStrings = Util::getJointStateStrings (vars);
  for (unsigned i = 0; i < jointDist.size(); i++) {
    cout << "P(" << jointStrings[i] << ") = " ;
    cout << setprecision (Constants::PRECISION) << jointDist[i];
    cout << endl;
  }
  cout << endl;
}


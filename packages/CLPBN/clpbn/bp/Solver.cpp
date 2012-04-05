#include "Solver.h"
#include "Util.h"


void
Solver::printAllPosterioris (void)
{
  const VarNodes& vars = fg_.varNodes();
  for (unsigned i = 0; i < vars.size(); i++) {
    printPosterioriOf (vars[i]->varId());
  }
}



void
Solver::printPosterioriOf (VarId vid)
{
  VarNode* vn = fg_.getVarNode (vid);
  const Params& posterioriDist = getPosterioriOf (vid);
  const States& states = vn->states();
  for (unsigned i = 0; i < states.size(); i++) {
    cout << "P(" << vn->label() << "=" << states[i] << ") = " ;
    cout << setprecision (Constants::PRECISION) << posterioriDist[i];
    cout << endl;
  }
  cout << endl;
}



void
Solver::printJointDistributionOf (const VarIds& vids)
{
  Vars vars;
  VarIds vidsWithoutEvidence;
  for (unsigned i = 0; i < vids.size(); i++) {
    VarNode* vn = fg_.getVarNode (vids[i]);
    if (vn->hasEvidence() == false) {
      vars.push_back (vn);
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


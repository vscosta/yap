#include "Solver.h"
#include "Util.h"


void
Solver::printAnswer (const VarIds& vids)
{
  Vars   unobservedVars;
  VarIds unobservedVids;
  for (unsigned i = 0; i < vids.size(); i++) {
    VarNode* vn = fg.getVarNode (vids[i]);
    if (vn->hasEvidence() == false) {
      unobservedVars.push_back (vn);
      unobservedVids.push_back (vids[i]);
    }
  }
  Params res = solveQuery (unobservedVids);
  vector<string> stateLines = Util::getStateLines (unobservedVars);
  for (unsigned i = 0; i < res.size(); i++) {
    cout << "P(" << stateLines[i] << ") = " ;
    cout << std::setprecision (Constants::PRECISION) << res[i];
    cout << endl;
  }
  cout << endl;
}



void
Solver::printAllPosterioris (void)
{
  VarIds vids;
  const VarNodes& vars = fg.varNodes();
  for (unsigned i = 0; i < vars.size(); i++) {
    vids.push_back (vars[i]->varId());
  }
  std::sort (vids.begin(), vids.end());
  for (unsigned i = 0; i < vids.size(); i++) {
    printAnswer ({vids[i]});
  }
}


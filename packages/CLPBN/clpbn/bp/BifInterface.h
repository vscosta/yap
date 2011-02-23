#ifndef BIF_INTERFACE_H
#define BIF_INTERFACE_H

using namespace std;

class BayesianNetwork;
class BayesianNode;

class BifInterface
{
  public:
    static void          createNetworkFromXML (BayesianNetwork*, const char*);

  private:
    static double*       reorderParameters (double*, int, int);
};

#endif // BIF_INTERFACE_H





class YAPError {
  int errNo;
public:
  YAPError() { errNo = YAP_NO_ERROR; };
  YAPError(int err) { errNo = err; };
  int get();
  const char *text();
};


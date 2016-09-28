
#define FORMAT_MAX_SIZE 1024

typedef struct {
  Int filler;
  /* character to dump */
  int phys;
  /* position in buffer */
  int log; /* columnn as wide chsh */
} gap_t;

typedef struct format_status {
  gap_t gap[16];
  // number of octets
  int phys_start;
  // number of characters
  int lstart;
  int gapi;
} format_info;

#define FORMAT_COPY_ARGS_ERROR -1
#define FORMAT_COPY_ARGS_OVERFLOW -2

extern bool format_synch(int sno, int sno0, format_info *fg);
extern bool fill_pads(int sno, int sno0, int total, format_info *fg USES_REGS);
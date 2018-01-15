#ifndef __FILTER_PROPERTIES__
#define __FILTER_PROPERTIES__

#include <opencv2/core.hpp>
using namespace cv;

typedef enum PART_TYPE {
  ROWS = 0,
  COLUMNS = 1
} PART_TYPE;

typedef enum FILTER_CATEGORY {
  MULTI_TRANS = 0,
  SINGLE_TRANS = 1,
  NO_TRANS = 2
} FILTER_CATEGORY;

class filter_properties{
  public:
  filter_properties(int size, int max, void(*fun_ptr)(int, int, int, Mat&), PART_TYPE type,
                    FILTER_CATEGORY cat)
      : part_counter(0),
        partition_size(size),
        max_parts(max),
        ptr_to_filter(fun_ptr),
        partition_type(type),
        category(cat) {}

  ~filter_properties() = default;

  int part_counter;
  int partition_size;
  int max_parts;    
  void (*ptr_to_filter)(int, int, int, Mat&);

  PART_TYPE partition_type;
  FILTER_CATEGORY category;  


};

#endif

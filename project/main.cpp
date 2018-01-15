#include "filter_properties.h"
#include <thread>
#include <string>
#include <iostream>
#include <vector>
#include <sstream>

#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>


using namespace cv;
using namespace std;



void apply_filter(int rows, int cols, Mat &image,
                  std::vector<std::vector<double>> filter, double factor) {
    if(filter.size() < 1)
      return;

    int rows_in_filter = filter.size();
    int cols_in_filter = filter[0].size();

    Vec3b intensity{0, 0, 0};
    for(int filter_rows = 0; filter_rows < rows_in_filter; filter_rows++)
      for(int filter_cols = 0; filter_cols < cols_in_filter; filter_cols++){
        int pixel_x = (rows + filter_rows - rows_in_filter/2 + image.rows) % image.rows;
        int pixel_y = (cols + filter_cols - cols_in_filter/2 + image.cols) % image.cols;
        intensity[0] += image.at<Vec3b>(pixel_x, pixel_y)[0] * filter[filter_rows][filter_cols] * factor;
        intensity[1] += image.at<Vec3b>(pixel_x, pixel_y)[1] * filter[filter_rows][filter_cols] * factor;
        intensity[2] += image.at<Vec3b>(pixel_x, pixel_y)[2] * filter[filter_rows][filter_cols] * factor;

      }
    image.at<Vec3b>(rows, cols) = intensity;
}


void blur_interface(int row, int col, int partition_size, Mat &image){
  std::vector<std::vector<double>> filter{{0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0},
                                          {0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0},
                                          {0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0},
                                          {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0},
                                          {0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0},
                                          {0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0},
                                          {0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0}};
  double factor = 1.0 / 25.0;
  
  int end_row = (partition_size / image.rows) * image.rows + row;
  for (int r = row; r < end_row && r < image.rows; r++) {
    for (int c = col; c < image.cols; c++) {
      apply_filter(r, c, image, filter, factor);
    }
  }
}


void greyscale(int row, int col, Mat &image){
  __transactional_atomic {
    double blue = image.at<Vec3b>(row, col)[0];
    double green = image.at<Vec3b>(row, col)[1];
    double red = image.at<Vec3b>(row, col)[2];

    double grey = (blue + green + red) / 3;

    image.at<Vec3b>(row, col)[0] = grey;
    image.at<Vec3b>(row, col)[1] = grey;
    image.at<Vec3b>(row, col)[2] = grey;
  }
}

void greyscale_interface(int row, int col, int partition_size, Mat &image){
  int end_row = (partition_size / image.rows) * image.rows + row;
  for (int r = row; r < end_row && r < image.rows; r++) {
    for (int c = col; c < image.cols; c++) {
      greyscale(r, c, image);
    }
  }
}


void mirror(int rows, int cols, Mat &image){
  Vec3b oldPosition{0, 0, 0};
  Vec3b newPosition{0, 0, 0};

  __transactional_atomic {
    oldPosition = image.at<Vec3b>(rows, cols);
    newPosition = image.at<Vec3b>(rows, image.cols - cols);

    image.at<Vec3b>(rows, cols) = newPosition;
    image.at<Vec3b>(rows, image.cols - cols) = oldPosition;
  }
}

void mirror_interface(int row, int col, int partition_size, Mat &image){
  int end_row = (partition_size / image.rows) * image.rows + row;
  for(int r = row; r < end_row && r < image.rows; r++){
    for(int c = col; c < image.cols/2; c++){
      mirror(r,c,image);
    }
  }
}

void flip(int rows, int cols, Mat &image) {
  __transactional_atomic {
    Vec3b oldPosition{0, 0, 0};
    Vec3b newPosition{0, 0, 0};

    oldPosition = image.at<Vec3b>(rows, cols);
    newPosition = image.at<Vec3b>(image.rows - rows, cols);

    image.at<Vec3b>(rows, cols) = newPosition;
    image.at<Vec3b>(image.rows - rows, cols) = oldPosition;
  }
}

void flip_interface(int row, int col, int partition_size, Mat &image){
  int end_col = (partition_size / image.cols) * image.cols + col;
  for(int r = row; r < image.rows/2; r++){
    for(int c = col; c < end_col && c < image.cols; c++){
      flip(r,c,image);
    }
  }
}

void work_filters(std::vector<filter_properties*> &filters, Mat &image){
  for(int i = 0; i < (int)filters.size();){
    int part;
    //__transaction_atomic{
      part = filters[i]->part_counter;
      filters[i]->part_counter++;
    //} 
    if(part > filters[i]->max_parts)
      i++;
    else{
      int row_number = (filters[i]->partition_size / image.rows) * image.rows;
      int col_number = (filters[i]->partition_size / image.cols) * image.cols;
      filters[i]->ptr_to_filter(part * row_number *
                                ((filters[i]->partition_type) == ROWS),
                                part * col_number *
                                ((filters[i]->partition_type) == COLUMNS),
                                filters[i]->partition_size,
                                image);
    }
  }
}

void start_menu(int& threads){
  std::cout << "Welcome the our image processor!\n";
  std::cout << "First of, how many threads do you want to run when processing your image?\n";
  std::cout << "Threads: ";
  
  while(!(cin >> threads)){
    std::cout << "Please try again.\n";
    std::cout << "Threads: ";
  }

  cin.clear();
  cin.ignore(INT_MAX, '\n');  
}


int load_image(Mat& image, std::string file){
  image = imread(file, IMREAD_COLOR);
  return !image.empty();
}

void request_image(Mat& image){
  std::string file;
  std::cout << "Enter image name, including file ending: ";
  getline(cin, file);
  while (!load_image(image, file)) {
    std::cout << "Could not read file please trye again: ";
    getline(cin, file);
  }
  cin.clear();
}


int image_menu(Mat& image){
  if(image.empty()){
    request_image(image);
  } else {
    std::cout << "Choose an option below:\n";
    std::cout << "1) Continue working on current image.\n";
    std::cout << "2) Change image.\n";
    std::cout << "3) Quit.\n";
    std::cout << "Your choice: ";

    int input = 0;
    while(!(cin >> input) || !(input >= 1 && input <= 3)){
      std::cout << "Please type 1, 2 or 3: "; 
    }
    cin.clear();
    cin.ignore(INT_MAX, '\n');  

    if (input == 2){
      image.release();
      request_image(image);
    }
    else if(input == 3) return -1;
    
  }

  return 0;
}



typedef enum FILTER_NAME {
  MIRROR = 0,
  FLIP = 1,
  BLUR = 2,
  GREYSCALE = 3
} FILTER_NAME;

void create_filter_selection(Mat& image, std::vector<filter_properties*> &filter_selection){
  if(!filter_selection.empty()) return;

  int size = 1000000; //Pixels per working thread on the image
  int max_partitions = image.rows * image.cols / size; //Maximum partitions per image per filter

  //Mirror filter
  filter_selection.push_back(
      new filter_properties(size, max_partitions, mirror_interface, ROWS, SINGLE_TRANS));

  //Flip filter
  filter_selection.push_back(
      new filter_properties(size, max_partitions, flip_interface, COLUMNS, SINGLE_TRANS));

  //Blur
  filter_selection.push_back(
      new filter_properties(size, max_partitions, blur_interface, ROWS, NO_TRANS));

  //Greyscale
  filter_selection.push_back(
      new filter_properties(size, max_partitions, greyscale_interface, ROWS, MULTI_TRANS));
}


void print_filter_menu(std::string optional_top_msg = ""){
  std::cout << optional_top_msg;
  std::cout << "Which filters do you want to apply?\n";
  std::cout << "1) Mirror image.\n";
  std::cout << "2) Flip image.\n";
  std::cout << "3) Blur image.\n";
  std::cout << "4) Greyscale image.\n";
  std::cout << "0) Quit.\n";
  std::cout << "Input space separated list (e.g 1 2 3 4) and press enter : ";


};


int request_filter(std::vector<int>& input, std::vector<filter_properties*> &filters){ 
  print_filter_menu();
  string temp_input = "";
  istringstream iss;

  while (true) {
    getline(cin, temp_input);
    iss.str(temp_input);

    int i;
    while(true){
      iss >> i;
      if(iss.fail()) break;
      if(i == 0) return -1;
      if(i < 0 || i > filters.size()) break;

      input.push_back(i - 1);
      if (!iss.good()) break;
    }
    iss.clear();
    iss.str("");
    cin.clear();

    if(input.size()) break;
    print_filter_menu("Input could not be read. Please try again.\n");
  }
  return 0;
}


void parse_input_to_filters(
    std::vector<filter_properties *> &filter_selection,
    std::vector<int> &user_input,
    std::vector<filter_properties *> &transactional_filters,
    std::vector<filter_properties *> &rest_filters) {

  std::vector<filter_properties*> temp_single_tran;

  for(int i : user_input){
   FILTER_CATEGORY cat = filter_selection[i]->category; 
    if (cat == MULTI_TRANS)
      transactional_filters.push_back(filter_selection[i]);
    else if(cat == SINGLE_TRANS)
      temp_single_tran.push_back(filter_selection[i]);
    else if(cat == NO_TRANS)
      rest_filters.push_back(filter_selection[i]);
  }

  if(temp_single_tran.size() > 0){
    transactional_filters.push_back(temp_single_tran[temp_single_tran.size()-1]);
    temp_single_tran.pop_back();
  }

  for(auto filter : temp_single_tran)
    rest_filters.push_back(filter);
}


void process_image(Mat &image,
                   std::vector<filter_properties*> &transactional_filters,
                   std::vector<filter_properties*> &rest_filters,
                   int thread_count) {
  std::thread *workers = new std::thread[thread_count];
  
  //Start the transactional filters and wait for them to join before continuing
  for(int i = 0; i < thread_count; i++){
    workers[i] = std::thread(work_filters, std::ref(transactional_filters), std::ref(image));
  }
  for(int i = 0; i < thread_count; i++)
    workers[i].join();

  //Start up the non-transactional filters and wait on join before rendering or whatnot
  for(int i = 0; i < thread_count; i++){
    workers[i] = std::thread(work_filters, std::ref(rest_filters), std::ref(image));
  }
  for(int i = 0; i < thread_count; i++)
    workers[i].join();

  delete[] workers;
}

void createWindow(std::string name){
  namedWindow(name, WINDOW_AUTOSIZE);  // Create a window for display.
  waitKey(0);
}

void render(Mat& image, std::string name){
    imshow(name, image);
    waitKey(5);
}

int main( int argc, char** argv )
{

  std::vector<filter_properties*> filter_selection;
  std::vector<filter_properties*> transactional_filters;
  std::vector<filter_properties*> rest_filters;
  std::vector<int> user_input;
  std::string window_name = "ImageWindow";
  int thread_count;
  Mat image;
  std::thread render_thread;
  render_thread = std::thread(createWindow, window_name);

  start_menu(thread_count);
  while(true){
    if(image_menu(image) == -1) break;
    create_filter_selection(image, filter_selection);
    if(request_filter(user_input, filter_selection) == -1) break;
    parse_input_to_filters(filter_selection, user_input, transactional_filters, rest_filters); 
    process_image(image, transactional_filters, rest_filters, thread_count);
    render(image, window_name);   
  }

  if (!image.empty()) 
    image.release();

  for(auto filter : filter_selection)
    delete filter;
  return 0;
}

#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include "opencv2/opencv.hpp"


#include "filter_properties.h"
#include <thread>
#include <string>
#include <iostream>
#include <sstream>
#include <vector>
using namespace cv;
using namespace std;

Mat rotateImage(double angle, Mat &oldImage)
{
    Mat src = oldImage;


    // get rotation matrix for rotating the image around its center
    Point2f center(src.cols/2.0, src.rows/2.0);
    Mat rot = getRotationMatrix2D(center, angle, 1.0);
    // determine bounding rectangle
    Rect bbox = RotatedRect(center,src.size(), angle).boundingRect();
    // adjust transformation matrix
    rot.at<double>(0,2) += bbox.width/2.0 - center.x;
    rot.at<double>(1,2) += bbox.height/2.0 - center.y;

    Mat dst;
    warpAffine(src, dst, rot, bbox.size());


    return dst;
}

std::vector<std::vector<double>> filter{
                            {0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0},
                            {0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0},
                            {0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0},
                            {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0},
                            {0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0},
                            {0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0},
                            {0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0}
};




void apply_filter(int rows, int cols, Mat &image){
    if(filter.size() < 1)
      return;

    int rows_in_filter = filter.size();
    int cols_in_filter = filter[0].size();
    double factor = 1.0 / 25.0;

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
void mirror(int rows, int cols, Mat &image){
        Vec3b oldPosition{0, 0, 0};
        Vec3b newPosition{0, 0, 0};
         oldPosition = image.at<Vec3b>(rows, cols);
         newPosition = image.at<Vec3b>(rows,image.cols-cols);


         image.at<Vec3b>(rows, cols) = newPosition;
         image.at<Vec3b>(rows,image.cols-cols) = oldPosition;
}
void flip(int rows, int cols, Mat &image){

          Vec3b oldPosition{0, 0, 0};
          Vec3b newPosition{0, 0, 0};

         oldPosition[0] = image.at<Vec3b>(rows, cols)[0];
         oldPosition[1] = image.at<Vec3b>(rows, cols)[1];
         oldPosition[2] = image.at<Vec3b>(rows, cols)[2];

         newPosition[0] = image.at<Vec3b>(image.rows-rows,cols)[0];
         newPosition[1] = image.at<Vec3b>(image.rows-rows,cols)[1];
         newPosition[2] = image.at<Vec3b>(image.rows-rows,cols)[2];


         image.at<Vec3b>(rows, cols)[0] = newPosition[0];
         image.at<Vec3b>(rows, cols)[1] = newPosition[1];
         image.at<Vec3b>(rows, cols)[2] = newPosition[2];
         image.at<Vec3b>(image.rows-rows,cols)[0] = oldPosition[0];
         image.at<Vec3b>(image.rows-rows,cols)[1] = oldPosition[1];
         image.at<Vec3b>(image.rows-rows,cols)[2] = oldPosition[2];

}

void mirror_all(int rows, int cols, Mat &image){
  for (int row = 0; row < rows; row++) {
      for(int col = 0; col < cols/2; col++){
        mirror(row, col, image);
      }
    }
}


void work_filters(std::vector<filter_properties*> &filters, Mat &image){
  for(int i = 0; i < (int)filters.size();){
    //trans_atomic{
    int part = filters[i]->part_counter;
    filters[i]->part_counter++;
    //} 
    if(part > filters[i]->max_parts)
      i++;
    else
      filters[i]->ptr_to_filter(part * (filters[i]->partition_size) *
                                ((filters[i]->partition_type) == ROWS),
                                part * (filters[i]->partition_size) *
                                ((filters[i]->partition_type) == COLUMNS),
                                image);
  }
}


void start_menu(int& threads){
  std::cout << "Welcome the our image processor!\n";
  std::cout << "First of, how many threads do you want to run when processing your image?\n";
  std::cout << "Threads: ";
  
  while(!cin >> threads){
    std::cout << "Please try again.\n";
    std::cout << "Threads: ";
  }

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
}


int image_menu(Mat& image){
  if(image.empty()){
    request_image(image);
  } else {
    std::cout << "Choose an option below:\n";
    std::cout << "1) Continue working on current image.\n";
    std::cout << "2) Change image.\n";
    std::cout << "3) Quit.\n";
    std::cout << "Your choise: ";

    int input = 0;
    while(!cin >> input || !(input >= 1 && input <= 3)){
      std::cout << "Please type 1, 2 or 3: "; 
    }

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
  int size = 1000000; //Pixels per working thread on the image
  int max_partitions = image.rows * image.cols / size; //Maximum partitions per image per filter

  //Mirror filter
  filter_selection.push_back(
      new filter_properties(size, max_partitions, mirror, ROWS, SINGLE_TRANS));

  //Flip filter
  filter_selection.push_back(
      new filter_properties(size, max_partitions, flip, COLUMNS, SINGLE_TRANS));

  //Blur
//  filter_selection.push_back(
//      new filter_properties(size, max_partitions, , ROWS, NO_TRANS));


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

void request_filter(std::vector<int>& input){
 
  print_filter_menu();

  int i;
  std::string temp_input;
  std::stringstream ss;
  
  while (true) {
    std::getline(std::cin, temp_input);
    ss << temp_input;

   /* while (ss >> i) {
      input.push_back(i);
    }
*/
    ss.clear();
    ss.str("");

    if(input.size()) break;
    print_filter_menu("Input could not be read. Please try again.\n");
  }
}


int sort_filters(std::vector<filter_properties*>& filter_selection,
                 std::vector<int>& user_input,
                 std::vector<filter_properties*>& transactional_filters,
                 std::vector<filter_properties*>& rest_filters) {

  return 0;
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

void render(Mat& image){
  namedWindow("SomeName", WINDOW_AUTOSIZE);  // Create a window for display.
  imshow("SomeName", image);
}

int main( int argc, char** argv )
{

  std::vector<filter_properties*> filter_selection;
  std::vector<filter_properties*> transactional_filters;
  std::vector<filter_properties*> rest_filters;
  std::vector<int> user_input;
  int thread_count;
  Mat image;

  while(true){
    start_menu(thread_count);
    if(image_menu(image) == -1) break;
    create_filter_selection(image, filter_selection);
    request_filter(user_input);
  //push filters to either trans_filters[] or non_trans_filters[]
    process_image(image, transactional_filters, rest_filters, thread_count);
    render(image);   
  }
  //delete

  if (!image.empty()) 
    image.release();

  //delete[] threads;

    return 0;
}

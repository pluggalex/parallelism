#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include <iostream>
#include <string>
#include <vector>
using namespace cv;
using namespace std;

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

int main( int argc, char** argv )
{
    String imageName( "lena.jpg" ); // by default
    if( argc > 1)
    {
        imageName = argv[1];
    }
    Mat image;
    image = imread( imageName, IMREAD_COLOR ); // Read the file
    if( image.empty() )                      // Check for invalid input
    {
        cout <<  "Could not open or find the image" << std::endl ;
        return -1;
    }


    for(int rows = 0; rows < image.rows; rows++){
      for(int cols = 0; cols < image.cols; cols++){
        apply_filter(rows, cols, image);
      }
    }
    
    namedWindow("Display window",
                WINDOW_AUTOSIZE);  // Create a window for display.
    imshow( "Display window", image );                // Show our image inside it.
    waitKey(0); // Wait for a keystroke in the window
    return 0;
}

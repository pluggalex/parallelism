#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>

#include <iostream>
#include <string>
#include <vector>
using namespace cv;
using namespace std;


void greyscale(int row, int col, Mat &image){
  double blue = image.at<Vec3b>(row, col)[0];
  double green = image.at<Vec3b>(row, col)[1];
  double red = image.at<Vec3b>(row, col)[2];

  double grey = (blue + green + red)/3;

  image.at<Vec3b>(row, col)[0] = grey;
  image.at<Vec3b>(row, col)[1] = grey;
  image.at<Vec3b>(row, col)[2] = grey;

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


    for(int row = 0; row < image.rows; row++){
      for(int col = 0; col < image.cols; col++){
        greyscale(row, col, image);
      }
    }
   
    namedWindow("Display window",
                WINDOW_AUTOSIZE);  // Create a window for display.
    imshow( "Display window", image );                // Show our image inside it.
    waitKey(0); // Wait for a keystroke in the window
    return 0;
}

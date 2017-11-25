class DQueue
{
  struct QNode
  {
    QNode *left, *right;
    int val;
  };

  QNode *leftSentinel, *rightSentinel;
  int Counter;
public:

  DQueue()
  {
    // TODO
    leftSentinel = new QNode;
    rightSentinel = new QNode;
    leftSentinel->right = rightSentinel;
    leftSentinel->left = nullptr;
    rightSentinel->left = leftSentinel;
    rightSentinel->right = nullptr;
    Counter = 0;
  }

  void PushLeft(int val)
  {
    __transaction_atomic{
      QNode *new_node = new QNode();
      new_node->val = val;

      QNode *temp_node = rightSentinel->left;
      rightSentinel->left = new_node;
      temp_node->right = new_node;
      new_node->right = rightSentinel;
      new_node->left = temp_node;
      Counter++;
    }
  }

  void PushRight(int val)
  {
    __transaction_atomic {
      QNode *new_node = new QNode();
      new_node->val = val;

      QNode *temp_node = leftSentinel->right;
      leftSentinel->right = new_node;
      temp_node->left = new_node;
      new_node->left = leftSentinel;
      new_node->right = temp_node;
      Counter++;
    }
  }

  int PopLeft()
  {
    __transaction_atomic {
      QNode *temp_node = leftSentinel->right->right;
      if (temp_node == nullptr) return -1;

      int val = leftSentinel->right->val;
      delete leftSentinel->right;
      leftSentinel->right = temp_node;
      temp_node->left = leftSentinel;
    
      Counter++;
      return val;
    }
  }

  int PopRight()
  {
    __transaction_atomic {
      QNode *temp_node = rightSentinel->left->left;
      if (temp_node == nullptr) return -1;

      int val = rightSentinel->left->val;
      delete rightSentinel->left;
      rightSentinel->left = temp_node;
      temp_node->right = rightSentinel;
    
      Counter++;
      return val;
    }
  }

  int get_counter(){return Counter;}

};

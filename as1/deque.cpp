class DQueue
{
  struct QNode
  {
    QNode *left, *right;
    int val;
  };

  QNode *leftSentinel, *rightSentinel;

public:

  DQueue()
  {
    // TODO
    leftSentinel->right = rightSentinel;
    leftSentinel->left = nullptr;
    rightSentinel->left = leftSentinel;
    rightSentinel->right = nullptr;
  }

  void PushLeft(int val)
  {
    // TODO
    QNode *new_node = new QNode();    
    new_node->val = val;
    
    QNode *temp_node = rightSentinel->left;
    rightSentinel->left = new_node;
    temp_node->right = new_node;
  }

  void PushRight(int val)
  {
    // TODO
    QNode *new_node = new QNode();    
    new_node->val = val;
    
    QNode *temp_node = leftSentinel->right;
    leftSentinel->right = new_node;
    temp_node->left = new_node;// TODO
  }

  int PopLeft()
  {
    // TODO
    return -1;
  }

  int PopRight()
  {
    // TODO
    return -1;
  }

};

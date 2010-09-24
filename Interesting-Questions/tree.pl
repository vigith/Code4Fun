## Question:
## Given an inorder and pre-order list of a tree, create a tree (post-order)

## LOGIC:
## the pre-order will have the root the sub-tree at any moment and
## in-order list can be used to find the left and right subtree for each
## root from the pre-order
##
## A, B, C, D, E, F, G, H, I (left, root, right) : Inorder
## F, B, A, D, C, E, G, I, H (root, left, right) : Preorder
## A, C, E, D, B, H, I, G, F (left, right, root) : Postorder

$\ = "\n";

my @pr = qw(F B A D C E G I H);

sub tree {
  my @in = @_;
  return if not @in;
  my $root  = shift @pr || return;
  my ($l, $r)  = divide($root, @in); 

  tree(@in[0..$l]); tree(@in[$r..$#in]); print $root;
  
  return;
}

sub divide {
  my $root = shift || return;

  my @arr = @_;
  my $count = 0;
  foreach (@arr) {
    if ($root ne $_) {
      $count += 1 
    } else {
      return $count - 1, $count + 1
    }
  }
}

tree(qw(A B C D E F G H I));
# print divide('F', qw(A B C D E F G H I));

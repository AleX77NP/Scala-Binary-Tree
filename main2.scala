// same thing, but with  pattern matching
object ScalaBinaryTreeDemo {
    def main(srgs: Array[String]) = {
        val tree: BinaryTree = new BinaryTree(Some(new Node(7)))
        tree.add(2)
        tree.add(22)
        tree.add(15)
        tree.add(0)
        tree.add(4)
        tree.add(9)

        BinaryTree.displayTree(tree.getRoot())
    }
}

class Node(var value: Int, var left: Option[Node], var right: Option[Node]) {
    this.setValue(value)
    this.setLeft(left)
    this.setRight(right)

    // setters
    def setValue(value: Int) = {
        this.value = value
    }

    def setLeft(left: Option[Node]) = {
        this.left = left
    }

    def setRight(right: Option[Node]) = {
        this.right = right
    }

    // getters
    def getValue(): Int = value
    def getLeft(): Option[Node] = left
    def getRight(): Option[Node] = right

    def this(value: Int) = {
        this(value, None, None)
    }
}

class BinaryTree(var root: Option[Node]) {
    this.setRoot(root)

    def setRoot(root: Option[Node]) = {
        this.root = root
    }

    private def addRecursive(current: Option[Node], value: Int) : Option[Node] =  current match {
        case None => return Some(new Node(value))
        case Some(current) => {
            if (value < current.getValue()) {
                current.setLeft(addRecursive(current.getLeft(), value))
            }
            else if (value > current.value) {
                current.setRight(addRecursive(current.getRight(), value))
            }
            else {
                return Some(current)
            }
            return Some(current)
        }
    }

    def add(value: Int) = {
        this.setRoot(addRecursive(root, value))
    }

    def getRoot(): Option[Node] = root
}

object BinaryTree {
    // print tree using recursion
    def displayTree(node: Option[Node]) : Unit = {
        if(node != None) {
            displayTree(node.get.left)
            println(node.get.value)
            displayTree(node.get.right)
        }
    }
}
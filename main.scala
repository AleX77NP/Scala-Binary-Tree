object ScalaBinaryTreeDemo {
    def main(srgs: Array[String]) = {
        val tree: BinaryTree = new BinaryTree(Some(new Node(7)))
        tree.add(3)
        tree.add(10)
        tree.add(13)
        tree.add(1)
        tree.add(3)
        tree.add(5)

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

    private def addRecursive(current: Option[Node], value: Int) : Option[Node] = {
        if(current == None) {
            return Some(new Node(value))
        }

        val currentNode = current.get

        if (value < currentNode.getValue()) {
            currentNode.setLeft(addRecursive(currentNode.getLeft(), value))
        }
        else if (value > currentNode.value) {
            currentNode.setRight(addRecursive(currentNode.getRight(), value))
        }
        else {
            return Some(currentNode)
        }

        return Some(currentNode)
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
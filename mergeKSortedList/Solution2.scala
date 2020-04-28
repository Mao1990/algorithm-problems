object Solution {
    def mergeKLists(lists: Array[ListNode]): ListNode = {
        @scala.annotation.tailrec
        def recGripElems(lists: List[ListNode], acc: List[Int]): List[Int] = {
            def recListNode(lsNode: ListNode, acc: List[Int]): List[Int] = lsNode match {
                case null => acc
                case n => recListNode(n.next, n.x :: acc)
            }
            lists match {
                case x :: xs => recGripElems(xs, recListNode(x, acc))
                case Nil => acc
            }
        }
        val allElems = recGripElems(lists.toList, Nil).sorted
        def listToListNode(ls: List[Int]): ListNode = ls match {
            case x :: xs => {
                val node = new ListNode(x)
                node.next = listToListNode(xs)
                node
            }
            case Nil => null
        }
        listToListNode(allElems)
    }
}

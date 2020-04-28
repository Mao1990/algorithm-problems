object Solution {
    def mergeKLists(lists: Array[ListNode]): ListNode = {
        val initNode = new ListNode()
        def nextNode(lists: Array[ListNode]): ListNode = {
            if (lists.exists(_ != null)) {
                var i = 0
                var min = Int.MaxValue
                for (n <- lists) {
                    if (n != null && n.x <= min) {
                        i = lists.indexOf(n)
                        min = n.x
                    }
                }
                lists(i) = lists(i).next
                val node = new ListNode(min)
                node.next = nextNode(lists)
                node
            } else {
                null
            }
        }
        initNode.next = nextNode(lists)
        initNode.next
    }
}

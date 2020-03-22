/**
 * Definition for singly-linked list.
 * class ListNode(var _x: Int = 0) {
 *   var next: ListNode = null
 *   var x: Int = _x
 * }
 */
object Solution {
    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
        val added = l1.x + l2.x
        val c = (l1.x + l2.x) / 10
        val ln = new ListNode(added % 10)
        ln.next = (l1.next, l2.next) match {
            case (null, null) => 
              if ((added / 10) > 0) new ListNode(added / 10) else null
            case (l1n, null) => addTwoNumbers(l1n, new ListNode(c))
            case (null, l2n) => addTwoNumbers(new ListNode(c), l2n)
            case (l1n, l2n) => addTwoNumbers(addTwoNumbers(l1n, l2n), new ListNode(c))
        }
        ln
    }
        
}

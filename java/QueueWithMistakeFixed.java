// Lock-based queue with memory management mistake
// sestoft@itu.dk * 2013-10-20
// nh@itu.dk - 2019-03-31:
//   Concurrency removed from example.
//   Now just a simple array with queues uesed.
//   Text below edited accordingly.

// The SentinelLockQueue is a simple first-in first-out buffer,
// implemented as a linked list of Nodes.  Method queue.put(x) inserts
// item x and method queue.get() removes and returns an item provided
// the queue is not empty.

// The queue implementation in class SentinelLockQueue has an extra
// "sentinel" Node so that the head and tail fields always have a Node
// to point to, even when the queue is empty.  This means that the
// first integer item in the queue is not head.item but head.next.item.

// The queue implementation contains a programming mistake that
// usually causes the test program to run out of memory quite soon
// although this should not happen.

// The mistake actually appears in an example in Goetz et al: Java
// Concurrency in Practice, 2006, page 334 (ch 15) and the book's
// errata and online source code do not correct it.  But in all other
// respects it is an extremely useful and recommendable book!

/*

Analysis of QueueWithMistake

Reference types (nodes in our case) are stored in the heap given their unpredictable lifetime. The nodes contain references to other nodes in the heap.

The programming mistake in QueueWithMistake is found the get() operation, that fails to null the reference to the next node from the current node being dequeued/popped. In the eyes of the collector, the entire list remains reachable from the root set. Given that they remain reachable, they are live and moved to an older generation, as is evident by the stack trace performing a minor GC event:

[0.123s][info][gc] GC(0) Pause Young (Normal) (G1 Evacuation Pause) 23M->22M(258M) 45.903ms

The individual nodes cost ~40 bytes, but with 50 queues and 20000000, each byte we use is scaled by a billion given linked list has O(N) space complexity, thereby potentially becoming gigabytes of allocated memory if we cannot compact dead references (~1gb per Queue).

The GC process also becomes slower when attempting to mark and sweep the heap, since traversing a linked list is O(N). The problem is further exacerbated during stop and copy performed by the runtime when it copies large lists when the heap gets filled.

Eventually, the JVM attempts to do a full GC to compact the heap once it is filled and generation 2 has old objects:

[13.204s][info][gc] GC(29) Pause Full (G1 Compaction Pause) 4093M->4004M(4096M) 1994.283ms

We are unsure if the JVM has the same concept of LOH as .NET but we believe each queue is also a candidate to be put on LOH because a queue object exceeds the 85000 byte threshold as shown previously.

Solution: before returning from get(), null the reference to the next node to prevent loitering. This allows the memory to be collected and re-allocated.
*/

class QueueWithMistakeFixed {
  public static void main(String[] args) {
      run ();
  }

  private static void run() {
    final int iterations = 20000000; // Increase this constant if program does not run out of memory.
    final int noQueues = 50; // Number of queues.
    final Timer timer = new Timer();
    Queue[] queues = new SentinelLockQueue[noQueues];
    for (int j=0; j<noQueues; j++) {
        queues[j] = new SentinelLockQueue();
        queues[j].put(42);
        for (int i=0; i<iterations; i++) {
            queues[j].put(i);
            queues[j].get();
        }
        System.out.printf("%-20s\tQno. %2d\t%7.2f\t%s%n", 
                          queues[j].getClass().getName(), j,
                          timer.Check(), queues[j].get());
    }
  }
}

interface Queue {
  boolean put(int item);
  int get();
}

// --------------------------------------------------
// Locking queue, with sentinel (dummy) node

class SentinelLockQueue implements Queue {  
  // With sentinel (dummy) node.
  // Invariants:
  //  * The node referred by tail is reachable from head.
  //  * If non-empty then head != tail, 
  //     and tail points to last item, and head.next to first item.
  //  * If empty then head == tail.

  private static class Node {
    final int item;
    volatile Node next;
    
    public Node(int item, Node next) {
      this.item = item;
      this.next = next;
    }
  }

  private final Node dummy = new Node(-444, null);
  private Node head = dummy, tail = dummy;
  
  public boolean put(int item) {
    Node node = new Node(item, null);
    tail.next = node;
    tail = node;
    return true;
  }

  public int get() {
    if (head.next == null) 
      return -999;
    Node first = head;
    head = first.next;
    int item = head.item;
    first.next = null;
    return head.item;
  }
}

// Crude timing utility ----------------------------------------
   
class Timer {
  private long start, spent = 0;
  public Timer() { Play(); }
  public double Check() { return (System.nanoTime()-start+spent)/1E9; }
  public void Pause() { spent += System.nanoTime()-start; }
  public void Play() { start = System.nanoTime(); }
}

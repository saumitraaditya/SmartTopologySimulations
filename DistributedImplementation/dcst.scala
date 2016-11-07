import akka.actor.{ActorSystem, ActorRef, Actor, Props,actorRef2Scala,PoisonPill,ActorLogging}
import collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit;
import scala.io.Source
import scala.util.Random
import util.control.Breaks._


case object start
case object revaluate_socialTopo
case object trigger_lact
case object displayTopology
case object BootStrap
case object GreedyPatch
// Random Topology
case object randomTopology
case class RVUpdate(senderID:Int,RV:ArrayBuffer[Int])
case class RVUpdateNeg(senderID:Int,TrLinks:ArrayBuffer[Int])
case class LACTupdate(senderID:Int,lact:collection.immutable.Set[edge])
case class Trimupdate(senderID:Int,lact:collection.immutable.Set[edge])
case class con_req(senderId:Int)
case class con_ack(senderID:Int)
case class trim_req(senderId:Int)
case class trim_ack(senderId:Int)
case class add_new_links(senderId:Int,req_link_set:ArrayBuffer[edge])
case class remove_old_links(senderId:Int,old_link_set:ArrayBuffer[edge])
//Bootstrap request/acks
case class bsr_req(senderID:Int)
case class bsr_ack(senderID:Int)
case class bsr_nack(senderID:Int)
// represents a link in the graph.
class edge(source:Int,target:Int,value:Double,trueLink:Boolean)
{
  val src = source;
  val dst = target;
  var cost = value;
  var real = trueLink;
  
  def getString():String=
  {
    return "<"+src+"--->"+dst+":cost "+cost+" status: "+real+">";
  }
  
  def canEqual(a:Any) = a.isInstanceOf[edge]
  
  override def equals(that:Any):Boolean = 
   that match {
    case that:edge => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }
  
  override def hashCode:Int = {
    val prime = 31
    var result = 1
    result = prime * result + src;
    result = prime * result + dst;
    return result
  }
}
/*BootStrapBookEntry*/
class BSBE(State:String,Timestamp:Long)
{
  var curr_state = State;
  var timestamp = Timestamp;
  
  def updateState(state:String):Unit={
    curr_state = state;
  }
  
  def updateTimer(timer:Long):Unit={
    timestamp = timer;
  }
}
/* This class represents unified view of the neighborhood, containing all information 
 * that would be needed by the DCST algorithm to work, each invocation of MinDCST
 * would be provided with a updated snapshot that reflects the network view in the 
 * neighborhood.*/
class ViewSnapshot(socialView:HashMap[Int,ArrayBuffer[Tuple2[Int,Double]]],realView:HashMap[Int,ArrayBuffer[Int]])
{
  var snapshot  = new HashMap [Int,ArrayBuffer[edge]]();
  val MaxVal = 10000.0;
  /* initialize snapshot-
   * 1. create storage
   * 2. populate social links
   * 3. identify real subset
   * 4. populate costs*/
  for (source <- socialView.keySet)
  {
    snapshot+=(source->new ArrayBuffer[edge]())
    for (target <- socialView(source))
    {
      //snapshot(source)+=new edge(source,target._1,MaxVal/(socialView(source).size+socialView(target._1).size),realView(source).contains(target._1))
        snapshot(source)+=new edge(source,target._1,MaxVal/(target._2),realView(source).contains(target._1))

    }
  } 
}
// number of actual links to/from the node
case class realDegree(num_links:Int)
// Link advert method
case class LinkAdvt(source:Int, target:Int)
// Exchange Rosters with Friends
case class RosterExchange(sender:Int,roster:ArrayBuffer[Tuple2[Int,Double]])

/* Keeps track of the topology overtime */
class Monitor(RosterFile:String) extends Actor
{
   var EdgeMap = new HashMap[Int,ArrayBuffer[edge]]()
   for (line <- Source.fromFile(RosterFile).getLines())
      {
        val split_array = line.split(" ");
        val src = split_array(0).toInt;
        EdgeMap+=(src-> new ArrayBuffer[edge]())
        for (i<- 1 to split_array.length-1)
          {
            var dst_cost = split_array(i).split("-")
            var dst = dst_cost(0).toInt
            var cost = dst_cost(1).toDouble
            EdgeMap(src)+=(new edge(src,dst,cost,false))
          }
      }
  def receive=
    {
      case RVUpdate(senderID:Int, rv:ArrayBuffer[Int])=>
        {
          for (e<- EdgeMap(senderID))
          {
            if (rv.contains(e.dst))
            {
              e.real = true;
            } // else put false--> forces RV to stay current
            else
            {
              e.real = false;
            }
          }
        }
      case `displayTopology`=>
        {
          for (src <- EdgeMap.keySet)
          {
            println("------------------ "+src+" ----------------------")
            var SB = new StringBuilder();
            for (e<-EdgeMap(src))
            {
              SB.append(e.getString())
              SB.append(" ");
            }
            println(SB);
            println("-----------real/social--------------")
            println(EdgeMap(src).filter { _.real}.size.toFloat/EdgeMap(src).size)
          }
          println("\n\n\n\n")
          println("------------------ "+"FILE"+" ----------------------")
          for (src <- EdgeMap.keySet)
          {          
            var SB_file = new StringBuilder();
            SB_file.append(src+" ");
            for (e<-EdgeMap(src))
            {
              if (e.real == true)
              {
                SB_file.append(e.dst)
                SB_file.append(" ");
              }
            }
            println(SB_file)
          }
          println("\n\n\n\n");
          println("------------------ "+"END OF FILE"+" ----------------------")
          
        }
      case `start`=>
        {
          /* schedule periodic displays*/
          val Asys = context.system;
          import Asys.dispatcher;
          Asys.scheduler.schedule(new FiniteDuration(240,SECONDS),new FiniteDuration(300,SECONDS),self,displayTopology)
        }
    }
}

/* A node is an independent network gateway*/
class Node(val uid:Int, var Roster:ArrayBuffer[Tuple2[Int,Double]]) extends Actor
{
  val myID = uid;
  val degree_constraint = 30;
  val gamma_cost = .75;
  var CostPrevTree:Double = Double.MaxValue;
  var refCounter = new HashMap[Int,Int](){ override def default(key:Int) = 0 } //keeps track how many nodes depend on my link to this dest.
  var prevTree:ArrayBuffer[edge]= new ArrayBuffer[edge]();
  /* will contain social links*/
  var socialView = new HashMap[Int,ArrayBuffer[Tuple2[Int,Double]]](){ override def default(key:Int) = new ArrayBuffer[Tuple2[Int,Double]] }
  /* will contain actual links, needs to be updated when new links are created by self, or information about
   * new link created by neighbors is learnt*/
  var realView = new HashMap[Int,ArrayBuffer[Int]](){ override def default(key:Int) = new ArrayBuffer[Int] }
  /*Initially, node will have edges to to immediate neighbors,
   * gradually it will learn via messaging about edges via neighbor
   * to shared neighbors and add them */
  socialView+=(uid->new ArrayBuffer[Tuple2[Int,Double]]());
  realView+=(uid->new ArrayBuffer[Int]());
  
  /* BootStrap bookkeeping*/
  var BSQ = 5;
  var BSC = 0; //BootStrapCounter, links created to count BSR links created.
  var BST = 5; //BootStrapThreshhold
  var BSR_outstanding=0;
  var BootStrapQ = new collection.mutable.Queue[Int] //needed to send boot strap requests in a round robin manner.
  var BootStrapBook = new HashMap[Int,BSBE](){ override def default(key:Int) = new BSBE("empty",0) }
  for (neighbor <- Roster)
  {
     socialView+=(neighbor._1->new ArrayBuffer[Tuple2[Int,Double]]()); // Adjacency list for neighbors
     realView+=(neighbor._1->new ArrayBuffer[Int]()); // realView for neighbors
     socialView(uid)+=neighbor; // Adjacency list for root node.
     var BSBE_temp = new BSBE("empty",0)
     BootStrapBook+=(neighbor._1->BSBE_temp);
     BootStrapQ+=neighbor._1
  }    
  def receive = 
  {
    // Advertisement for real links
    case LinkAdvt(source:Int,target:Int)=>
      {         
            realView(source)+=target;
      }
    case `start`=>
      {
        val Asys = context.system;
        import Asys.dispatcher;
        Asys.scheduler.schedule(new FiniteDuration(1,SECONDS),new FiniteDuration(30,SECONDS),self,revaluate_socialTopo)
        //randomTopology
        Asys.scheduler.schedule(new FiniteDuration(10,SECONDS),new FiniteDuration(80,SECONDS),self,trigger_lact)
        //Asys.scheduler.schedule(new FiniteDuration(10,SECONDS),new FiniteDuration(120,SECONDS),self,randomTopology)
        Asys.scheduler.schedule(new FiniteDuration(60,SECONDS),new FiniteDuration(100,SECONDS),self,GreedyPatch)
        //Asys.scheduler.schedule(new FiniteDuration(60,SECONDS),new FiniteDuration(120,SECONDS),self,BootStrap)
      }
    case `GreedyPatch`=>
      {
        /*Greedy Link set-up*/
         if (realView(myID).size < degree_constraint)
         {
           //select nodes with whom no direct link
           //arrange them in descending order of priority.
           //select a subset of them to send con_reqs.
           var GreedyTop = socialView(myID).toList.sortBy(_._2);
           var to_select = degree_constraint - realView(myID).size;
           var counter = 0
           breakable
           {
                 for(dst <- GreedyTop)
                 {
                   if (!realView(myID).contains(dst._1))
                   {
                     counter = counter+1;
                     //send con_req to dst
                      val requestTo = "../"+dst._1.toString;
                      context.actorSelection(requestTo) !  con_req(myID)
                   }
                   if (counter >= to_select)
                       break;
                 }
           }
           
         }
      }
    case `BootStrap`=>
      {
        
        if (BST > socialView(myID).size)
          BST = socialView(myID).size
        //if I already have more than or equal to threshhold links do nothing
        if (realView(myID).size < BST)
        {
          //links needed
          var needed_links = BST - realView(myID).size
          /* check if any of timers have expired or are still running for sent con_reqs*/
          for (key <- BootStrapBook.keySet)
          {
            var temp = BootStrapBook.getOrElse(key, new BSBE("empty",0));
            if (temp.curr_state.equals("sent_req"))
            {
              if ((System.currentTimeMillis/1000) - temp.timestamp > 60)
                {
                  BSR_outstanding = BSR_outstanding-1;
                  temp.updateState("empty")
                }
            }
            if (temp.curr_state.equals("bsr_nack"))
            {
                  BSR_outstanding = BSR_outstanding-1;
                  temp.updateState("empty")
            }
            
          }
          if (BSR_outstanding < 0)
            BSR_outstanding =0;// sanity check to play safe
          var can_send = needed_links - BSR_outstanding//can only send these many requests in this iteration
          
           /* Go through the queue */
          var i=0;
          var j = 0
          while ( i < can_send && j < BootStrapQ.size)
          {
            j+=1;
            //pluck from queue
            var candidate = BootStrapQ.dequeue();
            var entry = BootStrapBook.getOrElse(candidate,new BSBE("empty",0));
            if (realView(myID).contains(candidate))
            {
              entry.updateState("connected");
              BootStrapQ+=candidate
            }
            else
            {
              if (entry.curr_state.equals("empty"))
              {
                val requestTo = "../"+candidate.toString;
                context.actorSelection(requestTo) !  bsr_req(myID)
                entry.updateState("sent_req");
                entry.updateTimer(System.currentTimeMillis/1000);
                BootStrapQ+=candidate
                BSR_outstanding+=1;
                i+=1;
              }
            }
            
          }
        }
      }
    case `revaluate_socialTopo`=>
      {
        /*Simplified implementation -- send my roster to nodes in my roster.*/
        // create a deep copy of Roster and send it across
        val rosterCopy = Roster.clone();
        val RVclone = realView(myID).clone()
        for (neighbor <- Roster)
        {
          val neighborActor = "../"+neighbor._1.toString;
          context.actorSelection(neighborActor) ! RosterExchange(uid,rosterCopy)
          context.actorSelection(neighborActor) !  RVUpdate(myID,RVclone)
        }
         /* Inform monitor about changes in the topology*/
          val monitor = "../"+"monitor";
          context.actorSelection(monitor) !  RVUpdate(myID,RVclone)
      }
     case `trigger_lact` =>
      {
        /*Take a snapshot of the views
         * invoke LACT on the snapshot
         * when the results are obtained
         * update realView of self
         * disseminate the realView and 
         * DCST*/
        //ViewSnapshot(socialView:HashMap[Int,ArrayBuffer[Int]],realView:HashMap[Int,ArrayBuffer[Int]])
        var timedShot = new ViewSnapshot(socialView,realView)
        var myLact = new LACT(1,timedShot,.2,degree_constraint);
        val currentDCST:ArrayBuffer[edge] = myLact.iterateTree(myID);
        //myLact.displaySpanningTree(true);//-------------------------------------------------------- UNCOMMENT FOR SMALL GRAPHS.
        var costCurrentTree = myLact.getCost();
        if (costCurrentTree < gamma_cost*CostPrevTree)
        {
            // updatePrevCost
            CostPrevTree = costCurrentTree;
            /* Calculate diffs*/
            var existingSet = prevTree.toSet
            var proposedSet = currentDCST.toSet
            var newSet = proposedSet.diff(existingSet.intersect(proposedSet)) //add these links
            var oldSet = existingSet.diff(existingSet.intersect(proposedSet)) //remove these links
            prevTree = currentDCST;
            //updating realView-add
            for (e<-newSet)
            {
              if (e.src == myID && !realView(myID).contains(e.dst))
              {
                //send con_req to dst
                val requestTo = "../"+e.dst.toString;
                context.actorSelection(requestTo) !  con_req(myID)
              }
            }
            // remove
//            for (e<-oldSet)
//            {
//              if (e.src == myID && realView(myID).contains(e.dst))
//              {
//                //send trim_req to dst
//                val requestTo = "../"+e.dst.toString;
//                context.actorSelection(requestTo) !  trim_req(myID)
//              }
//            }
            //disseminate set of edges to be added-sendDeepCopy
            for (neighbor <- Roster)
            {
              val neighborActor = "../"+neighbor._1.toString;
              context.actorSelection(neighborActor) ! LACTupdate(myID,newSet) //currentDCST is a val i.e constant.
            }
            //diss3minate set of edges to be removed
//             for (neighbor <- Roster)
//            {
//              val neighborActor = "../"+neighbor.toString;
//              context.actorSelection(neighborActor) ! Trimupdate(myID,oldSet) //currentDCST is a val i.e constant.
//            }
        }
      }
    case `randomTopology` =>
      {
        // Select random edges from social topology and map to overlay links.
        var randTop = scala.util.Random.shuffle(socialView(myID).toList).take(degree_constraint)
        for (dst<-randTop)
          {
            if (!realView(myID).contains(dst._1))
            {
              //send con_req to dst
              val requestTo = "../"+dst._1.toString;
              context.actorSelection(requestTo) !  con_req(myID)
            }
          }
        
        
      }
      
    case RVUpdate(senderID:Int, rv:ArrayBuffer[Int])=>
      {
        /*received real links created by neighbor
         * update my real links view, I will only use information about links 
         * to a shared neighbor*/
        for (dst <-rv)
        {
          if (socialView.keySet.contains(dst) && !realView(senderID).contains(dst))
            realView(senderID)+=dst;
          // TBR------------------------------------- My picture of realview only tells me about real links
          // so when calculating excess it might be hard to identify nodes which are heavily loaded.
        }
      }
   case LACTupdate(senderID:Int,lact:Set[edge])=>
      {
        /* neighbors dcst recvd,based on it I might create a few links requested by the neighbor
         * simple policy-- if realLinks I have created are less than degree_constraint I will 
         * create new links else I will ignore the request.*/
        for (e<-lact)
        {
          if (e.src == myID && realView(myID).size < degree_constraint) // sender wants me to create this link for him.
            {
              if (!realView(myID).contains(e.dst))
                {
                  // Initiate con_req to create a link on behalf of the requestor
                  val requestTo = "../"+e.dst.toString;
                  context.actorSelection(requestTo) !  con_req(myID)
                }
            }
        }
      }
   case Trimupdate(senderID:Int,trimSet:Set[edge])=>
      {
        for (e<-trimSet)
        {
          if (e.src == myID) // sender wants me to create this link for him.
            {
              if (realView(myID).contains(e.dst))
                {
                  // Initiate trim_req to remove the link on behalf of requestor
                  val requestTo = "../"+e.dst.toString;
                  context.actorSelection(requestTo) !  trim_req(myID)
                }
            }
        }
        /* Since real view is updated let others know*/
        //----------------------------------------------------- TBR
        // should I update others about realView here or after LACT has been calculated.
      }
    case RosterExchange(sender:Int,neighborRoster:ArrayBuffer[Tuple2[Int,Double]])=>
      {
        //Initially only interested in links to shared neighbours.
        // Add to local view a relevant link.
        for (target<-neighborRoster)
        {
          if (socialView.keySet.contains(target._1) && !socialView(sender).contains(target._1)) //fix duplicate additions
          {
            socialView(sender)+=target;
          }         
        }
      }
    case con_req(senderId:Int)=>
      {
        /*Con req recvd , act on it based on POLICY*/
        if (realView(myID).size < degree_constraint && !realView(myID).contains(senderId))
        {
             //I will create this link, send ack to sender
            realView(myID)+=senderId;
            var temp = refCounter(senderId)
            refCounter.update(senderId,temp+1)
            val requestor = "../"+senderId.toString;
            context.actorSelection(requestor) !  con_ack(myID)       
        }
        if (realView(myID).contains(senderId))
        {
           realView(myID)+=senderId;
           var temp = refCounter(senderId)
           refCounter.update(senderId,temp+1)
        }
      }
    case con_ack(senderID:Int)=>
      {
        if (!realView(myID).contains(senderID))
        {
          realView(myID)+=senderID
          var temp = refCounter(senderID)
          refCounter.update(senderID,temp+1)
        }
      }
      
    case trim_req(senderId:Int)=>
      {
        //just remove the link to sender from my realview
        if (realView(myID).contains(senderId))
        {
          if (refCounter(senderId)==0)
          {
            realView(myID)-=(senderId)
            val requestor = "../"+senderId.toString;
            context.actorSelection(requestor) !  trim_ack(myID)
          }
          else
          {
            var temp = refCounter(senderId)
            refCounter.update(senderId,temp-1)
          }      
        }    
      }
     case trim_ack(senderId:Int)=>
      {
        //just remove the link to sender from my realview, he has already done it.
        if (realView(myID).contains(senderId))
        {
          realView(myID)-=(senderId)
        }      
      }
    case bsr_req(senderID:Int)=>{
      /*check if BSQ is not violated, if not send bsr_ack, increment BSC and add link to overlay*/
      if (BSC < BSQ && (!realView(myID).contains(senderID)))
      {
        BSC= BSC+1;
        realView(myID)+=senderID;
        refCounter(senderID)+=1
        val requestor = "../"+senderID.toString;
        context.actorSelection(requestor) !  bsr_ack(myID)
      }
      else
      {
        val requestor = "../"+senderID.toString;
        context.actorSelection(requestor) !  bsr_nack(myID)
      }
    }
    case bsr_ack(senderID:Int)=>
      {
        if (!realView(myID).contains(senderID))
        {
          realView(myID)+=senderID
          var temp = refCounter(senderID)
          refCounter.update(senderID,temp+1)
          BootStrapBook.getOrElse(senderID,new BSBE("empty",0)).updateState("connected");
        }
      }
    case bsr_nack(senderID:Int)=>
      {
        if (!realView(myID).contains(senderID))
        {
          BootStrapBook.getOrElse(senderID,new BSBE("empty",0)).updateState("bsr_nack");
        }
      }
    case remove_old_links(senderId:Int,old_link_set:ArrayBuffer[edge])=>
      {
         for (e<-old_link_set)
        {
          if (e.src == myID) // sender wants me to create this link for him.
            {
              if (realView(myID).contains(e.dst))
                {
                  // Initiate con_req to create a link on behalf of the requestor
                  val requestTo = "../"+e.dst.toString;
                  context.actorSelection(requestTo) !  trim_req(myID)
                }
            }
        }
      }
    case add_new_links(senderId:Int,req_link_set:ArrayBuffer[edge]) =>
    {
      for (e<-req_link_set)
        {
          if (e.src == myID && realView(myID).size < degree_constraint) // sender wants me to create this link for him.
            {
              if (!realView(myID).contains(e.dst))
                {
                  // Initiate con_req to create a link on behalf of the requestor
                  val requestTo = "../"+e.dst.toString;
                  context.actorSelection(requestTo) !  con_req(myID)
                }
            }
        }
    }
  
  }
}

class SimulationManager(topology:ArrayBuffer[ActorRef]) extends Actor
{  
  def receive =
  {
    case `start` =>
      {
        for (node <- topology)
        {
          node ! start
        }
      }
  } 
}

object SmartTopology
{
  def main(args:Array[String])
  {
    println("HelloX20")
    if (args.length<1)
      println("Please enter the name of Graph file.")
    else
    {
      val GraphFile = args(0);
      val actor_system = ActorSystem("SmartTopology")
      val nodesInNetwork = new ArrayBuffer[ActorRef]();
      // Read the file , initialize the nodes.
      for (line <- Source.fromFile(GraphFile).getLines())
      {
        var roster = new ArrayBuffer[Tuple2[Int,Double]]();
        val split_array = line.split(" ");
        val src = split_array(0).toInt;
        for (i<- 1 to split_array.length-1)
          {
            var dst_cost = split_array(i).split("-")
            var dst = dst_cost(0).toInt
            var cost = dst_cost(1).toDouble
            
            roster+=(new Tuple2(dst,cost))
          }
        nodesInNetwork+=actor_system.actorOf(Props(new Node(src,roster)),src.toString());
      }
      val monitor = actor_system.actorOf(Props(new Monitor(GraphFile)),"monitor")
      val Manager = actor_system.actorOf(Props(new SimulationManager(nodesInNetwork)),"Manager")
      Manager ! start;
      monitor ! start;
    }
  }
}

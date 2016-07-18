import akka.actor.{ActorSystem, ActorRef, Actor, Props,actorRef2Scala,PoisonPill,ActorLogging}
import collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit;
import scala.io.Source


case object start
case object revaluate_socialTopo
case object trigger_lact
case object displayTopology
case class RVUpdate(senderID:Int,RV:ArrayBuffer[Int])
case class RVUpdateNeg(senderID:Int,TrLinks:ArrayBuffer[Int])
case class LACTupdate(senderID:Int,lact:ArrayBuffer[edge])
case class con_req(senderId:Int)
case class con_ack(senderID:Int)
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
}
/* This class represents unified view of the neighborhood, containing all information 
 * that would be needed by the DCST algorithm to work, each invocation of MinDCST
 * would be provided with a updated snapshot that reflects the network view in the 
 * neighborhood.*/
class ViewSnapshot(socialView:HashMap[Int,ArrayBuffer[Int]],realView:HashMap[Int,ArrayBuffer[Int]])
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
      snapshot(source)+=new edge(source,target,MaxVal/(socialView(source).size+socialView(target).size),realView(source).contains(target))
    }
  } 
}
// number of actual links to/from the node
case class realDegree(num_links:Int)
// Link advert method
case class LinkAdvt(source:Int, target:Int)
// Exchange Rosters with Friends
case class RosterExchange(sender:Int,roster:ArrayBuffer[Int])

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
          EdgeMap(src)+=(new edge(src,split_array(i).toInt,0.0,false))
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
          
          
        }
      case `start`=>
        {
          /* schedule periodic displays*/
          val Asys = context.system;
          import Asys.dispatcher;
          Asys.scheduler.schedule(new FiniteDuration(120,SECONDS),new FiniteDuration(120,SECONDS),self,displayTopology)
        }
    }
}

/* A node is an independent network gateway*/
class Node(val uid:Int, var Roster:ArrayBuffer[Int]) extends Actor
{
  val myID = uid;
  val degree_constraint = 4;
  var CostPrevTree:Double = Double.MaxValue;
  /* will contain social links*/
  var socialView = new HashMap[Int,ArrayBuffer[Int]](){ override def default(key:Int) = new ArrayBuffer[Int] }
  /* will contain actual links, needs to be updates when new links are created by self, or information about
   * new link created by neighbors is learnt*/
  var realView = new HashMap[Int,ArrayBuffer[Int]](){ override def default(key:Int) = new ArrayBuffer[Int] }
  /*Initially, node will have edges to to immediate neighbors,
   * gradually it will learn via messaging about edges via neighbor
   * to shared neighbors and add them */
  socialView+=(uid->new ArrayBuffer[Int]());
  realView+=(uid->new ArrayBuffer[Int]());
  for (neighbor <- Roster)
  {
     socialView+=(neighbor->new ArrayBuffer[Int]()); // Adjacency list for neighbors
     realView+=(neighbor->new ArrayBuffer[Int]()); // realView for neighbors
     socialView(uid)+=neighbor; // Adjacency list for root node.
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
        Asys.scheduler.schedule(new FiniteDuration(1,SECONDS),new FiniteDuration(20,SECONDS),self,revaluate_socialTopo)
        Asys.scheduler.schedule(new FiniteDuration(10,SECONDS),new FiniteDuration(60,SECONDS),self,trigger_lact)
      }
    case `revaluate_socialTopo`=>
      {
        /*Simplified implementation -- send my roster to nodes in my roster.*/
        // create a deep copy of Roster and send it across
        val rosterCopy = Roster.clone();
        val RVclone = realView(myID).clone()
        for (neighbor <- Roster)
        {
          val neighborActor = "../"+neighbor.toString;
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
        var myLact = new LACT(1,timedShot,.2,5);
        val currentDCST:ArrayBuffer[edge] = myLact.iterateTree(myID);
        myLact.displaySpanningTree(true);
        var costCurrentTree = myLact.getCost();
        if (costCurrentTree < CostPrevTree)
        {
            // updatePrevCost
            CostPrevTree = costCurrentTree;
            //updating realView
            for (e<-currentDCST)
            {
              if (e.src == myID && !realView(myID).contains(e.dst))
              {
                //send con_req to dst
                val requestTo = "../"+e.dst.toString;
                context.actorSelection(requestTo) !  con_req(myID)
              }
            }
            //disseminate LACT-sendDeepCopy
            for (neighbor <- Roster)
            {
              val neighborActor = "../"+neighbor.toString;
              context.actorSelection(neighborActor) ! LACTupdate(myID,currentDCST.clone) //currentDCST is a val i.e constant.
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
    case LACTupdate(senderID:Int,lact:ArrayBuffer[edge])=>
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
        /* Since real view is updated let others know*/
        //----------------------------------------------------- TBR
        // should I update others about realView here or after LACT has been calculated.
      }
    case RosterExchange(sender:Int,neighborRoster:ArrayBuffer[Int])=>
      {
        //Initially only interested in links to shared neighbours.
        // Add to local view a relevant link.
        for (target<-neighborRoster)
        {
          if (socialView.keySet.contains(target) && !socialView(sender).contains(target)) //fix duplicate additions
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
            val requestor = "../"+senderId.toString;
            context.actorSelection(requestor) !  con_ack(myID)       
        }
      }
    case con_ack(senderID:Int)=>
      {
        if (!realView(myID).contains(senderID))
        {
          realView(myID)+=senderID
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
        var roster = new ArrayBuffer[Int]();
        val split_array = line.split(" ");
        val src = split_array(0).toInt;
        for (i<- 1 to split_array.length-1)
          roster+=(split_array(i).toInt)
        nodesInNetwork+=actor_system.actorOf(Props(new Node(src,roster)),src.toString());
      }
      val monitor = actor_system.actorOf(Props(new Monitor(GraphFile)),"monitor")
      val Manager = actor_system.actorOf(Props(new SimulationManager(nodesInNetwork)),"Manager")
      Manager ! start;
      monitor ! start;
    }
  }
}




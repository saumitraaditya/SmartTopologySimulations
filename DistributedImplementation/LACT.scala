

/**
 * @author sam
 */
import scala.collection.mutable.ArrayBuffer;
import collection.mutable.HashMap
import util.control.Breaks._
import scala.io.Source


class LACT (uid:Int,view:ViewSnapshot,reward_val:Double,deg_constraint:Int)
{
  var myView:ViewSnapshot = view;
  var AutomatonTable = new HashMap[Int,Automaton]();
  for (node_id <- myView.snapshot.keySet)
  {
    AutomatonTable+= node_id->new Automaton(node_id,myView,reward_val,deg_constraint);
  }
  val MaxDeg = deg_constraint;
  var SpanningTree = new ArrayBuffer[edge];
  var CostTree:Double = 0.0;
  var BestTree:ArrayBuffer[edge] = null;
  var MinTreeCost = Double.MaxValue;
  var inCompleteTree:Boolean = false
  
  
  def displaySpanningTree(BestSpanningTree:Boolean=false)=
  {
    var displayString = new StringBuilder();
    if (BestSpanningTree)
      {
        for (this_edge<-BestTree)
          displayString.append(" %d-->%d ".format(this_edge.src,this_edge.dst));
        displayString.append("<< %f >>".format(MinTreeCost));
      }
    else
      for (this_edge<-SpanningTree)
        displayString.append(" %d-->%d ".format(this_edge.src,this_edge.dst));
    System.out.println(displayString);
  }
  /*Invocation results in a degree constrained spanning tree*/
  def start(curr_node_uid:Int):ArrayBuffer[Double]=
  {
    val start_vert = curr_node_uid;
    var current_automaton = AutomatonTable(start_vert);
    SpanningTree = new ArrayBuffer[edge];
    CostTree = 0.0;
    var verticesInTree = new HashMap[Int,Boolean]() {override def default(key:Int)= false}
    verticesInTree+=(start_vert->true);
    var treeTraceList = new scala.collection.mutable.Stack[Automaton];
    treeTraceList.push(current_automaton);
    var action_probability = new ArrayBuffer[Double]();
    var selectedAction:selected_action = null;
    var dcstPossible:Boolean = true
    breakable {
        while (SpanningTree.size != myView.snapshot.keySet.size-1)
          {
            //update action_set to avoid cycle
            current_automaton.cycleAvoidance(verticesInTree);
            if (current_automaton.validateActionSet()==true)
            {
              selectedAction = current_automaton.selectAction();
              SpanningTree+=(selectedAction.sel_edge);
              CostTree = CostTree+selectedAction.sel_cost;
              verticesInTree+=selectedAction.sel_edge.dst->true;
              treeTraceList.push(AutomatonTable(selectedAction.sel_edge.dst))
              action_probability+=selectedAction.action_prob;
            } 
            // if previous automaton is not active or it has no more actions to select
            // trace back for a  active automaton with a non empty action set
            else
            {
              var chosenAutomaton:Automaton = null;
              breakable {
                while (!treeTraceList.isEmpty)
                {
                  var candidateAutomaton = treeTraceList.pop();
                  candidateAutomaton.cycleAvoidance(verticesInTree);
                  if (candidateAutomaton.validateActionSet()==true)
                  {
                    chosenAutomaton = candidateAutomaton;
                    treeTraceList.push(chosenAutomaton);
                    break;
                  }
                }
              }
              if (chosenAutomaton == null)
              {
                //System.out.println("DCST is not possible for this vertex.");
                dcstPossible = false
                break;
              }
              
              selectedAction = chosenAutomaton.selectAction();
              SpanningTree+=(selectedAction.sel_edge);
              CostTree = CostTree+selectedAction.sel_cost;
              verticesInTree+=selectedAction.sel_edge.dst->true;
              treeTraceList.push(AutomatonTable(selectedAction.sel_edge.dst))
              action_probability+=selectedAction.action_prob;
            }
            current_automaton = AutomatonTable(selectedAction.sel_edge.dst);
          }
    }
    if ((CostTree < MinTreeCost || inCompleteTree) && dcstPossible) //do not want to select a incomplete tree with lower cost.
      {
        MinTreeCost = CostTree;
        BestTree = SpanningTree;
        inCompleteTree = false;
      }
    else if (CostTree < MinTreeCost && inCompleteTree)
    {
      MinTreeCost = CostTree;
      BestTree = SpanningTree;
    }
    if (MinTreeCost==Double.MaxValue) //At this stage if dcst is not at all possible, have not been realized in any prev iteration.
      {
        inCompleteTree = true
        MinTreeCost = CostTree;
        BestTree = SpanningTree;
      }
    //System.out.println("Iteration Tree Cost "+CostTree)
    //System.out.println("minimum cost tree is " + MinTreeCost);
    // after every iteration all automatons should be refreshed
    for (automatonID <- AutomatonTable.keySet)
    {
      AutomatonTable(automatonID).refresh()
    }
      // returns list of probabilities of selected actions.
    return action_probability;
  }
  
  def iterateTree(curr_vert_ID:Int):ArrayBuffer[edge]=
  {
    var counter = 0;
    while (counter < 10)
    {
      start(curr_vert_ID);
      counter = counter + 1;
    }
    return BestTree;
  }
  
  def getCost():Double=
    {
      return MinTreeCost;
    }
  
}

/*object LACT_unitTest 
{
   def main(args:Array[String])
  {  
    // create a unit test for LACT and make sure it works.
    // once this step is completed move on to inetgrate it with dcst.
    //LACT (uid:Int,view:ViewSnapshot,reward_val:Double,deg_constraint:Int)
    var socialView = new HashMap[Int,ArrayBuffer[Tuple2[Int,Double]]](){ override def default(key:Int) = new ArrayBuffer[Tuple2[Int,Double]] }
    var realView = new HashMap[Int,ArrayBuffer[Int]](){ override def default(key:Int) = new ArrayBuffer[Int] }
    if (args.length<2)
      System.out.println("Please enter the names of link desciption files.")
    else
      {
        val SocialLinksFile = args(0);
        val RealLinksFile = args(1);
        for (line <- Source.fromFile(SocialLinksFile).getLines())
        {
          val split_array = line.split(" ");
          val src = split_array(0).toInt;
          val store = socialView(src)
          for (i<- 1 to split_array.length-1)
          {
            var dst_cost = split_array(i).split("-")
            var dst = dst_cost(0).toInt
            var cost = dst_cost(1).toDouble
            store+=new Tuple2(dst,cost)
          }
          socialView+=(src->store);
        }
        for (line <- Source.fromFile(RealLinksFile).getLines())
        {
          val split_array = line.split(" ");
          val src = split_array(0).toInt;
          val store = realView(src);
          for (i<- 1 to split_array.length-1)
            store+=split_array(i).toInt;
          realView+=(src->store);
        }
      }
    /*
     * This part prints out the read in input link files
     * 
    for (src <- socialView.keySet)
      {
        for (dst<-socialView(src))
          System.out.println(src+"->"+dst);
      }
    System.out.println("--------------------------------------")
    for (src <- realView.keySet)
      {
        for (dst<-realView(src))
          System.out.println(src+"->"+dst);
      }
      * */
    //class ViewSnapshot(socialView:HashMap[Int,ArrayBuffer[Int]],realView:HashMap[Int,ArrayBuffer[Int]])
     var mySnapshot = new ViewSnapshot(socialView,realView);
     var myLact = new LACT(1,mySnapshot,.2,5);
     /* The below snippet prints out the snapshot sent as 
      * input to LACT
      * 
     for (src <- mySnapshot.snapshot.keySet)
     {
       for (e<-mySnapshot.snapshot(src))
       {
         System.out.println(e.src,e.dst,e.cost,e.real);
       }
     }
     * */
     
    myLact.iterateTree(23);
    myLact.displaySpanningTree(true);
    }
   
}*/ //wrap the entire main object into comments.
  

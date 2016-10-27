import collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

class ActionSet(action:edge,prob:Double,avbl:Boolean)
{
  val link:edge = action;
  var action_prob = prob;
  var available = avbl;
}

class selected_action(selected_edge:edge,action_probability:Double,selected_edge_cost:Double)
{
  val sel_edge = selected_edge;
  val action_prob = action_probability;
  val sel_cost = selected_edge_cost;
}
class Automaton (uid:Int,view:ViewSnapshot,reward_val:Double,deg_constraint:Int)
{
  val reward = reward_val;
  val degree_constraint = deg_constraint;
  val id = uid;
  var dynamicCost= Double.MaxValue;
  var min_initial_action_cost = Double.MaxValue; 
  var sum_costs:Double = 0.0;
  var actionSet = new ArrayBuffer[ActionSet];
  var numActionsTaken:Int = 0;
  var active:Boolean = true;
  var costAction = new HashMap[edge,Double]();
  /*calculate minimum cost from the snapshot*/
  for (link <- view.snapshot(uid))
  {
    sum_costs = sum_costs + link.cost;
    if (link.cost <= min_initial_action_cost)
    {
      min_initial_action_cost = link.cost;
    }
  }
  /* Initialize action set*/
  for (link <- view.snapshot(uid))
  {
    assignCosts(link);
    actionSet+=(new ActionSet(link,sum_costs/costAction(link),true))
  }
  
  def assignCosts(link:edge)=
  {
    if (link.real)
    {
      costAction+=(link->scala.util.Random.nextDouble*min_initial_action_cost/2);
    }
    else
    {
      var numRealLinksSrc = view.snapshot(link.src).filter(_.real==true).size;
      var numRealLinksDst = view.snapshot(link.dst).filter(_.real==true).size;
      if (numRealLinksSrc <= degree_constraint && numRealLinksDst <= degree_constraint)
      {
         
          costAction+=(link->link.cost);
      }
      else
      {
         var excess= 0;
          // filter edges that are real, find sum-- for both source and dst of the link.
          excess = excess+numRealLinksSrc - degree_constraint
          excess = excess+numRealLinksDst - degree_constraint
          costAction+=(link->(1+excess/degree_constraint)*link.cost);
      }
    }
  }
  
  def scaleActionSet(scale:Boolean=true,action_sum:Double=0):Double=
  {
    var sum:Double = 0;
    for (action <-actionSet)
    {
      if (action.available)
      {
        sum = sum + action.action_prob;
      }
    }
    if (scale==true)
    {
      for (action <-actionSet)
      {
        if (action.available)
          action.action_prob = action.action_prob/sum;
      }
    }
    else
    {
      for (action <-actionSet)
      {
        if (action.available)
          action.action_prob = action.action_prob * action_sum;
      }
    }
    return sum;
  }
  
  def updateActionSet(chosen_action:ActionSet,to_reward:Boolean=true)
  {
    if (to_reward)
    {
      for (action <-actionSet)
      {
        if (action == chosen_action)
          action.action_prob = action.action_prob + reward * (1 - action.action_prob);
      }
    }
    else
    {
      for (action <-actionSet)
      {
        if (action == chosen_action)
          action.action_prob = action.action_prob  * (1 - reward);
      }
    }
  }
  
  def validateActionSet(root:Boolean=false):Boolean=
  {
    if (root && numActionsTaken >= degree_constraint)
    {
        active = false;
        return false;    
    }
    else if (numActionsTaken >= degree_constraint-1)
    {
      active = false;
      return false;
    }
    else
    {
      for (action <- actionSet)
        if (action.available)
          return true
      return false;
    }
  }
  
  def cycleAvoidance(inTree:HashMap[Int,Boolean])
  {
    for (action<-actionSet)
    {
      if (inTree(action.link.src) && inTree(action.link.dst))
        action.available = false;
    }
  }
  
  def selectAction():selected_action=
  {
    val action_prob_sum = scaleActionSet(true);
    // sort action set in descending order of probabilities
    actionSet = actionSet.sortWith(_.action_prob > _.action_prob);
    var choice = scala.util.Random.nextDouble();
    var selectedAction:ActionSet = null;
    var sum:Double = 0.0;
    /* Print choice of avialable actions to validate that action being selected is the 
     * one with low cost and high probability*/
//    println("--------ActionSet--------")
//    for (action <- actionSet)
//    {
//      println(action.link.src+"--->"+action.link.dst,action.action_prob,action.available,costAction(action.link),action.link.real)
//    }
    breakable {
        for (action <- actionSet)
        {
          if (action.available)
          {
            sum = sum + action.action_prob;
            if (sum >= choice)
            {
              selectedAction = action;
              if (costAction(selectedAction.link) <= dynamicCost)
              {
                updateActionSet(selectedAction,true);
                dynamicCost = costAction(selectedAction.link);
              }
              else
              {
                updateActionSet(selectedAction,false);
              }
              break;
            }
          }
        }
    }//breakable
//    println("break_prob :"+ sum + " Selected Action "+selectedAction.link.src+"-->"+selectedAction.link.dst,selectedAction.action_prob);
    var selected_prob = selectedAction.action_prob;
    var selected_edge_cost = costAction(selectedAction.link);
    scaleActionSet(false,action_sum = sum);
    var invitedVertexID =  selectedAction.link.dst; // Ensure that it returns the destination and not self's uid.
    numActionsTaken = numActionsTaken + 1;  
    return new selected_action(selectedAction.link,selected_prob,selected_edge_cost);
  }
  
  def refresh()
  {
    numActionsTaken = 0;
    for (action <- actionSet)
      action.available = true;
  }
 
}

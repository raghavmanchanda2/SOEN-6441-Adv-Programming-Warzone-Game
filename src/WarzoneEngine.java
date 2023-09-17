import controller.WarzoneEngineController;

public class WarzoneEngine {

	  private WarzoneEngineController warzoneEngineController;
	
	  public WarzoneEngine() {
		  warzoneEngineController = new WarzoneEngineController();
	  }
	  
	  
	  
	  public synchronized void gameStarts() {
		  
		  System.out.println("Rohit");
		// intializing war zone game comments
		// printout different features
		// 1. Map Features
		// 2. Single game mode between multiple players
		// 3. tournament mode between multiple players  
		  
		  
		  // get commands input from warZone controller file 
		  
		  switch(this.warzoneEngineController.getGameFeatureInputs()) {
		  case 1:
			  //  sends to map engine 
			  break;
		  case 2: 
			  // single game mode engines
			  break;
		  case 3: 
			 // tournament game mode engine
			  break;
		  case -1 :
			 // restart the game
		  
		  
		  
		  
		  }
 
	}
	  
	  
}

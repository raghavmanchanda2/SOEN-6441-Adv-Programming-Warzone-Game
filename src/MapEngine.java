import controller.MapEngineController;

public class MapEngine {
	
	public static String CURRENT_MAP ;
	private MapEngineController mapEngineController;
	
	
	
	public MapEngine() {
		mapEngineController = new MapEngineController();
	}
	
	private void printAvailableMapCommands() {
		System.out.println("Print Available Commands OF MAIN MAP commands");
		// 1 VALIDATE MAP  --- validatemap
		// 2 edit MAP or create -- editmap filename
		// 3 show map ----- showmap
		// 4 save map ---- savemap filename
		// 5 exit map engine
		
		
	}
	
	private void printEditMapCommands() {
		System.out.println("Print Available Commands OF EDIT MAP commands");
		//1. editcontinent -add continentID continentvalue -remove continentID
		//2. editcountry -add countryID continentID -remove countryID
		//3. editneighbor -add countryID neighborcountryID -remove countryID neighborcountryID
	}
	
	public void startMapEngine() {
		
		this.printAvailableMapCommands();
		
		if(mapEngineController.getMainMapCommandsFromUser().getStatusValue() != 200) {
			this.startMapEngine();
		}else {
			this.printEditMapCommands();
			mapEngineController.getEditMapCommandsFromUser();
		}
		
		
		 
		
	}

}

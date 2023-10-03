import controller.MapEngineController;
import logger.ConsoleWriter;
import logger.Logger;
import model.ResponseWrapper;
public class MapEngine {
	
	public static String D_CURRENT_MAP ;
	private MapEngineController d_mapEngineController;
	
	private Logger d_logger;
	private ConsoleWriter d_consoleWriter;
	
	
	
	public MapEngine() {
		d_mapEngineController = new MapEngineController();
		d_logger = new Logger();
		d_consoleWriter = new ConsoleWriter();
		d_logger.addObserver(d_consoleWriter);
	}
	
	private void printAvailableMapCommands() {
		d_logger.setLogMessage("\n******** Map Engine ********");
		d_logger.setLogMessage("->Map Commands Available:\n "
				+ "To edit an existing map or create a new map file : editmap filename(filename.map) \n "
				+ "To exit the Map Phase : exit \n"
				+ "**************************** \n"
				+ "->Enter Command to proceed");
	}
	
	private void printEditMapCommands() {
		//System.out.println("Print Available Commands OF EDIT MAP commands");
		d_logger.setLogMessage("->Commands available in edit map: \n"
				+ "  To add a continent : editcontinent -add continentID continentvalue \n"
				+ "  To remove a continent : editcontinent -remove continentID \n"
				+ "  To add a country : editcountry -add countryID continentID \n"
				+ "  To remove a country : editcountry -remove countryID \n"
				+ "  To add a neighbour : editneighbour -add countryID neighborcountryID \n"
				+ "  To remove a neighbour : editneighbour -remove countryID neighborcountryID \n"
				+ "  To show the map : showmap \n"
				+ "  To save the map : savemap filename \n"
				+ "  To validate the map : validatemap \n"
				+ "  To exit map engine : exit \n"
				+ "->Enter Command to proceed");
		
	}
	
	public void startMapEngine() {
		
		while(true) {
			this.printAvailableMapCommands();
			ResponseWrapper mainMapCommandResponse = d_mapEngineController.getMainMapCommandsFromUser();
			d_logger.setLogMessage(mainMapCommandResponse.getDescription());
				if (mainMapCommandResponse.getStatusValue() == 204){
					break;
				}else if(mainMapCommandResponse.getStatusValue() == 200) {
					while(true) {
						this.printEditMapCommands();
						ResponseWrapper editMapCommandResponse = d_mapEngineController.getEditMapCommandsFromUser();
						d_logger.setLogMessage(editMapCommandResponse.getDescription());
						if(editMapCommandResponse.getStatusValue() == 204) {
							break;
						}
					}
				}
		}
			
	}
}

import controller.MapEngineController;
import logger.ConsoleWriter;
import logger.Logger;
import model.ResponseWrapper;
public class MapEngine {
	
	public static String CURRENT_MAP ;
	private MapEngineController mapEngineController;
	
	private Logger logger;
	private ConsoleWriter consoleWriter;
	
	
	
	public MapEngine() {
		mapEngineController = new MapEngineController();
		logger = new Logger();
		consoleWriter = new ConsoleWriter();
		logger.addObserver(consoleWriter);
	}
	
	private void printAvailableMapCommands() {
		logger.setLogMessage("Main Commands Available\n "
				+ "1. editmap filename \n "
				+ "2. exit \n"
				+ "Enter Command");
	}
	
	private void printEditMapCommands() {
		System.out.println("Print Available Commands OF EDIT MAP commands");
		logger.setLogMessage("Commands available in edit map \n"
				+ "1.  editcontinent -add continentID continentvalue \n"
				+ "2.  editcontinent -remove continentID \n"
				+ "3.  editcountry -add countryID continentID \n"
				+ "4.  editcountry -remove countryID \n"
				+ "5.  editneighbour -add countryID neighborcountryID \n"
				+ "6.  editneighbour -remove countryID neighborcountryID \n"
				+ "7.  showmap \n"
				+ "8.  savemap filename \n"
				+ "9.  validatemap \n"
				+ "10. exit \n"
				+ "Enter Command");
		
	}
	
	public void startMapEngine() {
		
		while(true) {
			this.printAvailableMapCommands();
			ResponseWrapper mainMapCommandResponse = mapEngineController.getMainMapCommandsFromUser();
			logger.setLogMessage(mainMapCommandResponse.getDescription());
				if (mainMapCommandResponse.getStatusValue() == 204){
					break;
				}else if(mainMapCommandResponse.getStatusValue() == 200) {
					while(true) {
						this.printEditMapCommands();
						ResponseWrapper editMapCommandResponse = mapEngineController.getEditMapCommandsFromUser();
						logger.setLogMessage(editMapCommandResponse.getDescription());
						if(editMapCommandResponse.getStatusValue() == 204) {
							break;
						}
					}
				}
		}
			
	}
}

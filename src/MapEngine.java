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
		d_logger.setLogMessage("Main Commands Available\n "
				+ "1. editmap filename \n "
				+ "2. exit \n"
				+ "Enter Command");
	}
	
	private void printEditMapCommands() {
		System.out.println("Print Available Commands OF EDIT MAP commands");
		d_logger.setLogMessage("Commands available in edit map \n"
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

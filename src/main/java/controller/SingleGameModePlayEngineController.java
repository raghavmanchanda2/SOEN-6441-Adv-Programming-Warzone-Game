package controller;

import java.util.Scanner;

import business.Phase;
import business.SingleGamePlayerCommands;
import logger.GeneralException;
import model.ResponseWrapper;

/**
 * SingleGameModePlayEngineController class
 * @author ishaanbajaj
 * @author Raghav
 * @version build 2
 */
public class SingleGameModePlayEngineController {

	/**
	 * scanner
	 */
	private Scanner d_inputForInitialSetupCommands;
	/**
	 * General Exception
	 */
	private GeneralException gException;
	/**
	 * Incorrect command string
	 */
	public static final String INCORRECT_COMMAND="Please enter proper command";
	/**
	 * Load Map String
	 */
	private static final String LOAD_MAP_FIRST = "Launch Load Map command first";
	/**
	 * boolean for load map first
	 */
	private static Boolean loadMapFirst=false;
	/**
	 * Object of SingleGamePlayerCommands
	 */
	private SingleGamePlayerCommands singleGamePlayerCommands;
	/**
	 * Object of Phase class - singlePlayPhase
	 */
	Phase singlePlayPhase;

	/**
	 * Default Constructor
	 */
	public SingleGameModePlayEngineController() {
		d_inputForInitialSetupCommands = new Scanner(System.in);
		gException=new GeneralException();
		singleGamePlayerCommands = new SingleGamePlayerCommands();
		setPlayPhase(singleGamePlayerCommands);
		}

	/**
	 * method to srt phase to singlePlayPhase
	 * @param singlePlayPhase play phase
	 */
	public void setPlayPhase(Phase singlePlayPhase) {
		this.singlePlayPhase = singlePlayPhase;
	}

	/**
	 * method to get command from user
	 * @return string
	 */
	public String getPlaySetupCommandsFromUser() {
		
		return d_inputForInitialSetupCommands.nextLine();
		
	}

	/**
	 * method to get setup commands from user
	 * @param l_userEnteredInitialSetupCommands string command
	 * @return response
	 * @throws GeneralException if anything goes wrong
	 */
	public ResponseWrapper getPlaySetupCommands(String l_userEnteredInitialSetupCommands) throws GeneralException {
		
		if (l_userEnteredInitialSetupCommands.trim().isEmpty()) {
			
			return new ResponseWrapper(404, INCORRECT_COMMAND); 
		}
		String[] l_splitInitialSetupCommand = l_userEnteredInitialSetupCommands.trim().replaceAll(" +", " ").split("\\s+");
		
		gException.validateCommand(l_userEnteredInitialSetupCommands);
		
		switch (l_splitInitialSetupCommand[0]) {

		case "loadmap":
			System.out.print("in load map");
			loadMapFirst=true;
			return singlePlayPhase.loadMap(l_splitInitialSetupCommand[1]);
			
			
		case "showmap":
			if(Boolean.FALSE.equals(loadMapFirst))
			{
				return new ResponseWrapper(404, LOAD_MAP_FIRST);
			}
			return singlePlayPhase.showMap();
			
			
			
		case "gameplayer":
			
			if(Boolean.FALSE.equals(loadMapFirst))
			{
				return new ResponseWrapper(404, LOAD_MAP_FIRST);
			}
			
			switch (l_splitInitialSetupCommand[1]) {

			case "-add":
				return singlePlayPhase.addPlayerInGame(l_splitInitialSetupCommand[2]);
				// call business file to add player
				

			case "-remove":
				return singlePlayPhase.removeplayerFromGame(l_splitInitialSetupCommand[2]);
				// call business file to remove
				
				
				
			default:
				return new ResponseWrapper(404, INCORRECT_COMMAND);

			}

			
		case "assigncountries":
			
			if(Boolean.FALSE.equals(loadMapFirst))
			{
				return new ResponseWrapper(404, LOAD_MAP_FIRST);
			} else if (singleGamePlayerCommands.getD_mapFileAlteration().getGameModel().getPlayers() == null
					|| singleGamePlayerCommands.getD_mapFileAlteration().getGameModel().getPlayers().size() == 1) {
				return new ResponseWrapper(404, "Player should be atleast 2 to assign countries");
			}
			
			return singlePlayPhase.assignCountries();
			
			
		case "continue":
			if (Boolean.FALSE.equals(loadMapFirst)) {
				return new ResponseWrapper(404, LOAD_MAP_FIRST);
			} else if (singleGamePlayerCommands.getD_mapFileAlteration().getGameModel().getPlayers() == null
					|| singleGamePlayerCommands.getD_mapFileAlteration().getGameModel().getPlayers().size() == 1) {
				return new ResponseWrapper(404, "Player should be atleast 2");
			}else if(singleGamePlayerCommands.getD_mapFileAlteration().getGameModel().getPlayers().get(0).getCountriesHold() == null) {
				return new ResponseWrapper(404, "Run assign countries command first");
			}
			
			return new ResponseWrapper(201,"Move to Next Phase");
			
			
		case "savemap":
			if(Boolean.FALSE.equals(loadMapFirst))
			{
				return new ResponseWrapper(404, LOAD_MAP_FIRST);
			}
			
			return singlePlayPhase.saveMap(null);
			

		case "validatemap":
			
			if(Boolean.FALSE.equals(loadMapFirst))
			{
				return new ResponseWrapper(404, LOAD_MAP_FIRST);
			}	
			
			return singlePlayPhase.validateMap();
			
		case "deploy":
			
			return singlePlayPhase.deploy(null, null, 0);
			
		case "advance":
			
			return singlePlayPhase.advance(null, null, null, 0);
			
		case "bomb":
			
			return singlePlayPhase.bomb(null, null);
			
		case "editcountry":
			
			return singlePlayPhase.editCountry(null, null);
			
		case "editneighbour":
			return singlePlayPhase.editNeighbour(null, null, null);
		
		case "editcontinent":
			return singlePlayPhase.editContinent(null, null);

			case "exit":
				System.out.println("Ending Game!");
				System.exit(0);
			
		default:
			return new ResponseWrapper(404, INCORRECT_COMMAND); // nothing entered please enter proper
																			// command
		}

	}
	

}

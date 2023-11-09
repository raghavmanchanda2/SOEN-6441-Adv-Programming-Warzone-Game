package controller;

import java.util.Scanner;

import business.MainPlayPhaseBusinessCommands;
import business.Phase;
import logger.GeneralException;
import model.Player;
import model.ResponseWrapper;

/**
 * MainPlayPhaseController class
 * @author ishaanbajaj
 * @author kevin
 * @author rohit
 * @version build 2
 */

public class MainPlayPhaseController {


	/**
	 * scanner
	 */
	private Scanner d_MainPlaylSetupCommands;
	/**
	 * General Execption object
	 */
	private GeneralException gException;
	/**
	 * string for incorrect command
	 */
	public static final String INCORRECT_COMMAND="Please enter proper command";
	/**
	 * Object of MainPlayPhaseBusinessCommands
	 */
	private MainPlayPhaseBusinessCommands mainPlayPhaseBusinessCommands;
	/**
	 * Object of class Phase - playPhase
	 */
	private Phase playPhase;

	/**
	 * Default Constructor
	 */
	public MainPlayPhaseController() {
		
		d_MainPlaylSetupCommands = new Scanner(System.in);
		gException=new GeneralException();
		mainPlayPhaseBusinessCommands = new MainPlayPhaseBusinessCommands();
		setPlayPhase(mainPlayPhaseBusinessCommands);
		
	}

	/**
	 * method to set phase as play phase
	 * @param playPhase single main play phase
	 */
	public void setPlayPhase(Phase playPhase) {
		this.playPhase = playPhase;
	}

	/**
	 * method to get the command from player
	 * @param currentPlayer Current Turn Player
	 * @return response
	 * @throws GeneralException if something goes wrong
	 */
	public ResponseWrapper getMainPlaySetUpCommandsFromUser(Player currentPlayer) throws GeneralException {
		String l_userEnteredMainPlayCommands = d_MainPlaylSetupCommands.nextLine();

		if (l_userEnteredMainPlayCommands.trim().isEmpty()) {
			
			return new ResponseWrapper(404, INCORRECT_COMMAND); 
		}
		String[] l_splitInitialSetupCommand = l_userEnteredMainPlayCommands.trim().replaceAll(" +", " ").split("\\s+");
		
		gException.validateCommand(l_userEnteredMainPlayCommands);
		
		switch (l_splitInitialSetupCommand[0]) {

			case "commit":
				return playPhase.commit(currentPlayer);

		case "deploy":
			
			return playPhase.deploy(currentPlayer,l_splitInitialSetupCommand[1], Integer.parseInt(l_splitInitialSetupCommand[2]));
			
		case "advance":
			return playPhase.advance(currentPlayer,l_splitInitialSetupCommand[1],l_splitInitialSetupCommand[2],Integer.parseInt(l_splitInitialSetupCommand[3]));
		
		case "bomb":
			return playPhase.bomb(currentPlayer, l_splitInitialSetupCommand[1]);
			
		case "blockade":
			
			return playPhase.blockade(currentPlayer,l_splitInitialSetupCommand[1]);
			
		case "airlift":
			
			return playPhase.airlift(currentPlayer, l_splitInitialSetupCommand[1], l_splitInitialSetupCommand[2], Integer.parseInt(l_splitInitialSetupCommand[3]));
		
		case "negotiate":
			
			return playPhase.diplomacy(currentPlayer, l_splitInitialSetupCommand[1]);
			
		case "loadmap":
			return playPhase.loadMap(null);
			
		case "gameplayer":
			
			return playPhase.addPlayerInGame(null);
			
		case "assigncountries":
			return playPhase.assignCountries();
			
		case "showmap":
			
			
			return playPhase.showMap();
			
			
		case "savemap":
			
			return playPhase.saveMap(null);
			

		case "validatemap":
			
			return playPhase.validateMap();
		
		case "editcountry":
			
			return playPhase.editCountry(null, null);
			
		case "editneighbour":
			return playPhase.editNeighbour(null, null, null);
		
		case "editcontinent":
			return playPhase.editContinent(null, null);

			case "exit":
				System.out.println("Player exit. Ending Game Now!");
				System.exit(0);

		default:
			return new ResponseWrapper(404, INCORRECT_COMMAND); 
		}
		

	}
	
	
	

}

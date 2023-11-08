package controller;

import java.util.Scanner;

import business.Phase;
import business.SingleGamePlayerCommands;
import logger.GeneralException;
import model.ResponseWrapper;

public class SingleGameModePlayEngineController {
	
	private Scanner d_inputForInitialSetupCommands;
	private GeneralException gException;
	public static final String INCORRECT_COMMAND="Please enter proper command";
	private SingleGamePlayerCommands singleGamePlayerCommands;
	Phase singlePlayPhase;

	public SingleGameModePlayEngineController() {
		d_inputForInitialSetupCommands = new Scanner(System.in);
		gException=new GeneralException();
		singleGamePlayerCommands = new SingleGamePlayerCommands();
		setPlayPhase(singleGamePlayerCommands);
		}
	
	public void setPlayPhase(Phase singlePlayPhase) {
		this.singlePlayPhase = singlePlayPhase;
	}
	
	public ResponseWrapper getPlaySetupCommandsFromUser() throws GeneralException {
		
		String l_userEnteredInitialSetupCommands = d_inputForInitialSetupCommands.nextLine();

		if (l_userEnteredInitialSetupCommands.trim().isEmpty()) {
			
			return new ResponseWrapper(404, INCORRECT_COMMAND); 
		}
		String[] l_splitInitialSetupCommand = l_userEnteredInitialSetupCommands.trim().replaceAll(" +", " ").split("\\s+");
		
		gException.validateCommand(l_userEnteredInitialSetupCommands);
		
		switch (l_splitInitialSetupCommand[0]) {

		case "loadmap":
			System.out.print("in load map");
			return singlePlayPhase.loadMap(l_splitInitialSetupCommand[1]);
			
			
		case "showmap":
			return singlePlayPhase.showMap();
			
			
			
		case "gameplayer":
			
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
			
			return singlePlayPhase.assignCountries();
			
			
		case "continue":
			return new ResponseWrapper(201,"Move to Next Phase");
			
			
		case "savemap":
			
			return singlePlayPhase.saveMap(null);
			

		case "validatemap":
			
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

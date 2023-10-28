package controller;

import java.util.Scanner;

import business.ExecuteMapsCommands;
import business.SingleGamePlayerCommands;
import logger.GeneralException;
import model.Continent;
import model.ResponseWrapper;

public class SingleGameModePlayEngineController {
	
	private Scanner d_inputForInitialSetupCommands;
	private GeneralException gException;
	public static final String INCORRECT_COMMAND="Please enter proper command";
	private ExecuteMapsCommands d_executeMapsCommands;
	private SingleGamePlayerCommands singleGamePlayerCommands;

	public SingleGameModePlayEngineController() {
		d_inputForInitialSetupCommands = new Scanner(System.in);
		gException=new GeneralException();
		d_executeMapsCommands = new ExecuteMapsCommands();
		singleGamePlayerCommands = new SingleGamePlayerCommands();
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
			return singleGamePlayerCommands.loadMap(l_splitInitialSetupCommand[1]);
			
			
		case "showmap":
			return d_executeMapsCommands.showMap();
			
			
			
		case "gameplayer":
			
			switch (l_splitInitialSetupCommand[1]) {

			case "-add":
				return singleGamePlayerCommands.addPlayerInGame(l_splitInitialSetupCommand[2]);
				// call business file to add player
				

			case "-remove":
				return singleGamePlayerCommands.removeplayerFromGame(l_splitInitialSetupCommand[2]);
				// call business file to remove
				
				
				
			default:
				return new ResponseWrapper(404, INCORRECT_COMMAND);

			}

			
		case "assigncountries":
			
			return singleGamePlayerCommands.assignCountries();
			
			
		case "continue":
			return new ResponseWrapper(201,"Move to Next Phase");
			
		default:
			return new ResponseWrapper(404, INCORRECT_COMMAND); // nothing entered please enter proper
																			// command
		}

	}
	

}

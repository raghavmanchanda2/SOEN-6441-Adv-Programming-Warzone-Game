/**
 * The `MapEngineController` class is responsible for handling map-related commands and interactions between the user interface and the map editing functionality.
 * It processes user input and delegates commands to the `ExecuteMapsCommands` class for map manipulation.
 *
 * <p>
 * This class supports commands for editing continents, countries, neighbors, showing the map, saving the map, and validating the map.
 * </p>
 *
 * @see ExecuteMapsCommands
 * @author Rohit
 * @version 1.0
 */
package controller;

import java.util.Scanner;

import business.ExecuteMapsCommands;
import business.Phase;
import logger.GeneralException;
import logger.LogGenerator;
import model.Continent;
import model.Country;
import model.ResponseWrapper;

/**
 * The `MapEngineController` class is responsible for handling map-related commands and interactions between the user interface and the map editing functionality.
 * It processes user input and delegates commands to the `ExecuteMapsCommands` class for map manipulation.
 *
 * <p>
 * This class supports commands for editing continents, countries, neighbors, showing the map, saving the map, and validating the map.
 * </p>
 *
 * @see ExecuteMapsCommands
 * @author Rohit
 * @version build 2
 */
public class MapEngineController {

	private Scanner d_inputForMapCommands;
	private ExecuteMapsCommands d_executeMapsCommands;
	private LogGenerator d_logGenrator;
	private Phase mapPhase;
	
	/**
	 * default error message for incorrect input command
	 */
	public static final String INCORRECT_COMMAND="Please enter proper command";
	private GeneralException gException=new GeneralException();

	/**
     * Enumerates the supported map-related commands.
     */
	enum MapCommands {
		EDIT_CONTINENT("editcontinent"), EDIT_COUNTRY("editcountry"), EDIT_NEIGHBOR("editneighbor"),
		SHOW_MAP("showmap"), SAVE_MAP("savemap"), EDIT_MAP("editmap"), VALIDATE_MAP("validatemap"), NONE("nothing");

		public final String l_exactCommand;

		MapCommands(String p_exactCommand) {
			this.l_exactCommand = p_exactCommand;
		}
		
		
	}

	public void setMapPhase(Phase mapPhase) {
		this.mapPhase = mapPhase;
	}

	/**
     * Default Constructor for the `MapEngineController` class.
     * Initializes the input scanner, map command executor, and log generator.
     */
	public MapEngineController() {
		d_inputForMapCommands = new Scanner(System.in);
		d_executeMapsCommands = new ExecuteMapsCommands();
		this.d_logGenrator = new LogGenerator();
		setMapPhase(d_executeMapsCommands);
	}

	/**
     * Gets the main map commands from the user and processes them.
     *
     * @return A {@link ResponseWrapper} indicating the result of the command execution.
     * @throws GeneralException If there is an error during command execution.
     */
	public ResponseWrapper getMainMapCommandsFromUser() throws GeneralException {
		
		String l_userEnteredMainMapCommands = d_inputForMapCommands.nextLine();

		if (l_userEnteredMainMapCommands.trim().isEmpty()) {
			
			return new ResponseWrapper(404, INCORRECT_COMMAND); // nothing entered please enter proper
																			// command
		}
		String[] l_splitMainMapCommand = l_userEnteredMainMapCommands.trim().replaceAll(" +", " ").split("\\s+");
		
		gException.validateCommand(l_userEnteredMainMapCommands);
		
		switch (l_splitMainMapCommand[0]) {

		case "editmap":
			
			ResponseWrapper mapResponse = mapPhase.editOrCreateMap(l_splitMainMapCommand[1]);
			ResponseWrapper l_response= mapPhase.validateMap();
			if(l_response.getStatusValue()==404)
			{
				System.out.println("Validation Failed : " + l_response.getDescription());
			}
			else
			{
				System.out.println(l_response.getDescription());

			}
			return mapResponse;
		case "continue":
			
			
			d_logGenrator.clearLogs();
			return new ResponseWrapper(201, "Return from current command and move to game play");
			
			
		default:
			return new ResponseWrapper(404, INCORRECT_COMMAND); // nothing entered please enter proper
																			// command
		}

	}

	 /**
     * Gets the edit map commands from the user and processes them.
     *
     * @return A {@link ResponseWrapper} indicating the result of the command execution.
     * @throws GeneralException If there is an error during command execution.
     */
	public ResponseWrapper getEditMapCommandsFromUser() throws GeneralException {

		String l_userEnteredCommand = d_inputForMapCommands.nextLine();
		if (l_userEnteredCommand.isEmpty()) {
			return new ResponseWrapper(404, INCORRECT_COMMAND);
		}
		// Filter
		String[] l_splittedCommands = l_userEnteredCommand.trim().replaceAll(" +", " ").split("\\s+");
		gException.validateCommand(l_userEnteredCommand);
		
		switch (l_splittedCommands[0]) {
		// edit continents
		case "editcontinent":
			
				
				switch (l_splittedCommands[1]) {

				case "-add":
					
					// call business file to execute command
					if (l_splittedCommands.length == 4) {
						Continent l_continent = new Continent(l_splittedCommands[2], l_splittedCommands[3]);
						return d_executeMapsCommands.addContinent(l_continent);
					}else {
						return new ResponseWrapper(404, INCORRECT_COMMAND);
					}

				case "-remove":
					if (l_splittedCommands.length == 3) {
						Continent l_continent = new Continent(l_splittedCommands[2]);
						return d_executeMapsCommands.removeContinent(l_continent);
					}else {
						return new ResponseWrapper(404, INCORRECT_COMMAND);
					}
					// call business file to execute command
					
				default:
					return new ResponseWrapper(404, INCORRECT_COMMAND);

				}

		// edit country
		case "editcountry":
			switch (l_splittedCommands[1]) {
			case "-add":

				if (l_splittedCommands.length == 4) {
					Country l_country = new Country(l_splittedCommands[2], new Continent(l_splittedCommands[3]));
					return d_executeMapsCommands.addCountry(l_country);
				} else {
					// please provide proper parameters
					return new ResponseWrapper(404, INCORRECT_COMMAND);
				}

				
			case "-remove":
				if (l_splittedCommands.length == 3) {
					Country l_country = new Country(l_splittedCommands[2]);
					return d_executeMapsCommands.removeCountry(l_country);
				} else {
					// please provide proper parameters
					return new ResponseWrapper(404, INCORRECT_COMMAND);
				}
				
			}
			break;

		case "editneighbour":
			switch (l_splittedCommands[1]) {
			case "-add":
				if (l_splittedCommands.length == 4) {
					Country l_country = new Country(l_splittedCommands[2]);
					Country l_neighbourCountry = new Country(l_splittedCommands[3]);
					return d_executeMapsCommands.addNeighbour(l_country, l_neighbourCountry);

				} else {
					return new ResponseWrapper(404, INCORRECT_COMMAND);
				}
				
			case "-remove":
				if (l_splittedCommands.length == 4) {
					Country l_country = new Country(l_splittedCommands[2]);
					Country l_neighbourCountry = new Country(l_splittedCommands[3]);
					return d_executeMapsCommands.removeNeighbour(l_country, l_neighbourCountry);

				} else {
					return new ResponseWrapper(404, INCORRECT_COMMAND);
				}
		
			}
			break;
		case "showmap":
			
			
			return mapPhase.showMap();
			
			
		case "savemap":
			
			return mapPhase.saveMap(l_splittedCommands[1]);
			

		case "validatemap":
			
			return mapPhase.validateMap();
			
		case "exit":
			
			d_logGenrator.clearLogs();
			return new ResponseWrapper(204, "Return from current command");
			
		case "deploy":
			
			return mapPhase.deploy(null, null, 0);
			
		case "advance":
			
			return mapPhase.advance(null, null, null, 0);
			
		case "bomb":
			
			return mapPhase.bomb(null, null);
			
		case "loadmap":
			
			return mapPhase.loadMap(null);
			
		case "gameplayer":
			
			return mapPhase.addPlayerInGame(l_userEnteredCommand);
			
		case "assigncountries":
			
			return mapPhase.assignCountries();
		
		default:
			return new ResponseWrapper(404, INCORRECT_COMMAND);


		}

		return new ResponseWrapper(404, INCORRECT_COMMAND);

	}

}

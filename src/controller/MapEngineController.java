package controller;

import java.util.Scanner;

import business.ExecuteMapsCommands;
import model.Continent;
import model.Country;
import model.ResponseWrapper;

public class MapEngineController {

	private Scanner d_inputForMapCommands;
	private ExecuteMapsCommands d_executeMapsCommands;

	enum MapCommands {
		EDIT_CONTINENT("editcontinent"), EDIT_COUNTRY("editcountry"), EDIT_NEIGHBOR("editneighbor"),
		SHOW_MAP("showmap"), SAVE_MAP("savemap"), EDIT_MAP("editmap"), VALIDATE_MAP("validatemap"), NONE("nothing");

		public final String l_exactCommand;

		MapCommands(String p_exactCommand) {
			this.l_exactCommand = p_exactCommand;
		}
	}

	public MapEngineController() {
		d_inputForMapCommands = new Scanner(System.in);
		d_executeMapsCommands = new ExecuteMapsCommands();
	}

	public ResponseWrapper getMainMapCommandsFromUser() {
		System.out.println("Getting input from user");
		String l_userEnteredMainMapCommands = d_inputForMapCommands.nextLine();

		if (l_userEnteredMainMapCommands.trim().isEmpty()) {
			System.out.println("No input from the user");
			return new ResponseWrapper(404, "Please enter proper command"); // nothing entered please enter proper
																			// command
		}
		String[] l_splitMainMapCommand = l_userEnteredMainMapCommands.trim().replaceAll(" +", " ").split("\\s+");

		switch (l_splitMainMapCommand[0]) {

		case "editmap":
			System.out.println("calling business file for editmap");
			return d_executeMapsCommands.editOrCreateMap(l_splitMainMapCommand[1]);
		case "exit":
			return new ResponseWrapper(204, "Return Form current command");
		default:
			return new ResponseWrapper(404, "Please enter proper command"); // nothing entered please enter proper
																			// command
		}

	}

	public ResponseWrapper getEditMapCommandsFromUser() {

		String l_userEnteredCommand = d_inputForMapCommands.nextLine();
		if (l_userEnteredCommand.isEmpty()) {
			return new ResponseWrapper(404, "Please enter proper command");
		}
		// Filter
		String[] l_splittedCommands = l_userEnteredCommand.trim().replaceAll(" +", " ").split("\\s+");
		
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
						return new ResponseWrapper(404, "Please enter proper command");
					}

				case "-remove":
					if (l_splittedCommands.length == 3) {
						Continent l_continent = new Continent(l_splittedCommands[2]);
						return d_executeMapsCommands.removeContinent(l_continent);
					}else {
						return new ResponseWrapper(404, "Please enter proper command");
					}
					// call business file to execute command
					
				default:
					return new ResponseWrapper(404, "Please enter proper command");

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
				}

				break;
			case "-remove":
				if (l_splittedCommands.length == 3) {
					Country l_country = new Country(l_splittedCommands[2]);
					return d_executeMapsCommands.removeCountry(l_country);
				} else {
					// please provide proper parameters
				}
				break;
			}
			break;

		case "editneighbour":
			switch (l_splittedCommands[1]) {
			case "-add":
				if (l_splittedCommands.length == 4) {
					System.out.println("calling business file editeighbour");
					Country l_country = new Country(l_splittedCommands[2]);
					Country l_neighbourCountry = new Country(l_splittedCommands[3]);
					return d_executeMapsCommands.addNeighbour(l_country, l_neighbourCountry);

				} else {
					// please provide proper parameters
				}
				break;
			case "-remove":
				if (l_splittedCommands.length == 4) {
					Country l_country = new Country(l_splittedCommands[2]);
					Country l_neighbourCountry = new Country(l_splittedCommands[3]);
					return d_executeMapsCommands.removeNeighbour(l_country, l_neighbourCountry);

				} else {
					// please provide proper parameters
				}

				break;
			}
			break;
		case "showmap":
			
			System.out.println("calling business file for showmap");
			return d_executeMapsCommands.showMap();
			
			
		case "savemap":
			System.out.println("calling business file for savemap");
			return d_executeMapsCommands.saveMap(l_splittedCommands[1]);
			

		case "validatemap":
			System.out.println("calling business file for validate map");
			return d_executeMapsCommands.validateMap();
			
		case "exit":
			return new ResponseWrapper(204, "Return Form current command");
		
		default:
			return new ResponseWrapper(404, "Please enter proper command");


		}

		return new ResponseWrapper(404, "Please enter proper command");

	}

}

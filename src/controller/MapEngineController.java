package controller;

import java.util.Scanner;

import business.ExecuteMapsCommands;
import model.Continent;
import model.Country;
import model.ResponseWrapper;

public class MapEngineController {

	private Scanner inputForMapCommands;
	private ExecuteMapsCommands executeMapsCommands;

	enum MapCommands {
		EDIT_CONTINENT("editcontinent"), EDIT_COUNTRY("editcountry"), EDIT_NEIGHBOR("editneighbor"),
		SHOW_MAP("showmap"), SAVE_MAP("savemap"), EDIT_MAP("editmap"), VALIDATE_MAP("validatemap"), NONE("nothing");

		public final String exactCommand;

		MapCommands(String exactCommand) {
			this.exactCommand = exactCommand;
		}
	}

	public MapEngineController() {
		inputForMapCommands = new Scanner(System.in);
		executeMapsCommands = new ExecuteMapsCommands();
	}

	public ResponseWrapper getMainMapCommandsFromUser() {
		System.out.println("Getting input from user");
		String userEnteredMainMapCommands = inputForMapCommands.nextLine();

		if (userEnteredMainMapCommands.trim().isEmpty()) {
			System.out.println("No input from the user");
			return new ResponseWrapper(404, "Please enter proper command"); // nothing entered please enter proper
																			// command
		}
		String[] splitMainMapCommand = userEnteredMainMapCommands.trim().replaceAll(" +", " ").split("\\s+");

		switch (splitMainMapCommand[0]) {

		case "editmap":
			System.out.println("calling business file for editmap");
			return executeMapsCommands.editOrCreateMap(splitMainMapCommand[1]);
			
		case "showmap":
			
			System.out.println("calling business file for showmap");
			return executeMapsCommands.showMap();
			
			
		case "savemap":
			System.out.println("calling business file for savemap");
			return executeMapsCommands.saveMap(splitMainMapCommand[1]);
			

		case "validatemap":
			System.out.println("calling business file for validate map");
			return executeMapsCommands.validateMap();
			

		default:
			return new ResponseWrapper(404, "Please enter proper command"); // nothing entered please enter proper
																			// command
		}

	}

	public MapCommands getEditMapCommandsFromUser() {

		String userEnteredCommand = inputForMapCommands.nextLine();
		if (userEnteredCommand.isEmpty()) {
			return MapCommands.NONE;
		}
		// Filter
		String[] splittedCommands = userEnteredCommand.trim().replaceAll(" +", " ").split("\\s+");

		switch (splittedCommands[0]) {

		// edit continents
		case "editcontinent":
			if (splittedCommands.length == 4) {
				Continent continent = new Continent(splittedCommands[3], splittedCommands[4]);
				switch (splittedCommands[1]) {

				case "-add":
					// call business file to execute command
					executeMapsCommands.addContinent(continent);

					break;

				case "-remove":
					// call business file to execute command
					executeMapsCommands.removeContinent(continent);
					break;

				default:
					return MapCommands.NONE;
				}

			} else {
				return MapCommands.NONE;
			}

			break;

		// edit country
		case "editcountry":
			switch (splittedCommands[1]) {
			case "-add":

				if (splittedCommands.length == 4) {
					Country country = new Country(splittedCommands[3], new Continent(splittedCommands[4]));
					executeMapsCommands.addCountry(country);
				} else {
					// please provide proper parameters
				}

				break;
			case "-remove":
				if (splittedCommands.length == 3) {
					Country country = new Country(splittedCommands[3]);
					executeMapsCommands.removeCountry(country);
				} else {
					// please provide proper parameters
				}
				break;
			}
			break;

		case "editneighbor":
			switch (splittedCommands[1]) {
			case "-add":
				if (splittedCommands.length == 4) {
					Country country = new Country(splittedCommands[3]);
					Country neighbourCountry = new Country(splittedCommands[4]);
					executeMapsCommands.addNeighbour(country, neighbourCountry);

				} else {
					// please provide proper parameters
				}
				break;
			case "-remove":
				if (splittedCommands.length == 4) {
					Country country = new Country(splittedCommands[3]);
					Country neighbourCountry = new Country(splittedCommands[4]);
					executeMapsCommands.removeNeighbour(country, neighbourCountry);

				} else {
					// please provide proper parameters
				}

				break;
			}
			break;
		
		default:
			return MapCommands.NONE;

		}

		return MapCommands.NONE;
	}

}

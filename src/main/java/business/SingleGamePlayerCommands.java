package business;


import GamePhase.MapPhaseState;
import logger.GeneralException;
import model.Continent;
import model.Country;
import model.Player;
import model.ResponseWrapper;
import persistence.GameModelAlteration;
import persistence.MapFileAlteration;

public class SingleGamePlayerCommands extends Phase{
	
	
	private MapFileAlteration d_mapFileAlteration;
	
	private GameModelAlteration gameModelAlteration;
	
	public SingleGamePlayerCommands() {
		d_mapFileAlteration = new MapFileAlteration();
		gameModelAlteration = new GameModelAlteration();
		
	}
	
	@Override
	public ResponseWrapper loadMap(String map) {
		
		MapPhaseState.D_CURRENT_MAP = map;
		d_mapFileAlteration.readMapFile();
		
		return this.d_mapFileAlteration.validateMap();
		
	}
	
	@Override
	public ResponseWrapper addPlayerInGame(String playerName) throws GeneralException {
		
		return this.gameModelAlteration.addPlayerInGame(playerName);
	}

	@Override
	public ResponseWrapper removeplayerFromGame(String playerName) throws GeneralException {
		
		return this.gameModelAlteration.removeplayerFromGame(playerName);
	}

	@Override
	public ResponseWrapper assignCountries() throws GeneralException {
		
		return this.gameModelAlteration.assignCountries();
	}

	@Override
	public ResponseWrapper advance(Player currentPlayer, String countryNameFrom, String countryNameTo,
			int numerOfarmies) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper showMap() throws GeneralException {
		return this.d_mapFileAlteration.showmap();
	}

	@Override
	public ResponseWrapper deploy(Player currentPlayer, String country, int numerOfarmies) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editContinent(Continent p_continent, String command) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper afterCommitReinforcement() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editNeighbour(Country p_mainCountry, Country p_neighbourCountry, String command)
			throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editCountry(Country p_country, String command) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper validateMap() throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper saveMap(String p_mapFileName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper editOrCreateMap(String p_mapFileName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper bomb(Player currentPlayer, String targetCountryName) throws GeneralException {
		return printInvalidCommandInState();
	}

	@Override
	public ResponseWrapper blockade(Player currentPlayer, String targetCountryName) throws GeneralException {
		return printInvalidCommandInState();
	}
	
	@Override
	public ResponseWrapper airlift(Player currentPlayer, String countryNameFrom, String countryNameTo, int numArmies)  throws GeneralException {
		return printInvalidCommandInState();
	}
	
	@Override
	public ResponseWrapper doReinforcements() throws GeneralException {
		return printInvalidCommandInState();
	}

	
	
	

}

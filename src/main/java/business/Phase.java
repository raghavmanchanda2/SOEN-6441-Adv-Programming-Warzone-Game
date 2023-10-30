package business;

import logger.GeneralException;
import logger.Logger;
import model.Continent;
import model.Country;
import model.Player;
import model.ResponseWrapper;

public abstract class Phase {

	private Logger d_logger;
	
	public Phase() {
		d_logger = new Logger();
	}
	public abstract ResponseWrapper advance(Player currentPlayer, String countryNameFrom, String countryNameTo,
			int numerOfarmies) throws GeneralException;

	public abstract ResponseWrapper showMap() throws GeneralException;


	public abstract ResponseWrapper deploy(Player currentPlayer, String country, int numerOfarmies)
			throws GeneralException;

	public ResponseWrapper printInvalidCommandInState() {
		return new ResponseWrapper(404, "Incorrect Command in Current State");
	}

	public abstract ResponseWrapper assignCountries() throws GeneralException;

	public abstract ResponseWrapper editContinent(Continent p_continent, String command) throws GeneralException;

	public abstract ResponseWrapper addPlayerInGame(String playerName) throws GeneralException;

	public abstract ResponseWrapper removeplayerFromGame(String playerName) throws GeneralException;

	public abstract ResponseWrapper afterCommitReinforcement()
			throws GeneralException;

	public abstract ResponseWrapper editNeighbour(Country p_mainCountry, Country p_neighbourCountry, String command)
			throws GeneralException;

	public abstract ResponseWrapper editCountry(Country p_country, String command) throws GeneralException;

	public abstract ResponseWrapper validateMap() throws GeneralException;

	public abstract ResponseWrapper loadMap(String map) throws GeneralException;

	public abstract ResponseWrapper saveMap(String p_mapFileName) throws GeneralException;

	public abstract ResponseWrapper editOrCreateMap(String p_mapFileName) throws GeneralException;

	public abstract ResponseWrapper bomb(Player currentPlayer, String targetCountryName) throws GeneralException;
	
	public abstract ResponseWrapper blockade(Player currentPlayer, String targetCountryName) throws GeneralException;

	public abstract ResponseWrapper doReinforcements() throws GeneralException;

}

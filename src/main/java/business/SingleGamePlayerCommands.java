package business;



import GamePhase.MapPhaseState;


import model.ResponseWrapper;
import persistence.GameModelAlteration;
import persistence.MapFileAlteration;

public class SingleGamePlayerCommands {
	
	
	
	
	
	private MapFileAlteration d_mapFileAlteration;
	
	private GameModelAlteration gameModelAlteration;
	
	public SingleGamePlayerCommands() {
		d_mapFileAlteration = new MapFileAlteration();
		gameModelAlteration = new GameModelAlteration();
		
	}
		
	public ResponseWrapper loadMap(String map) {
		
		MapPhaseState.D_CURRENT_MAP = map;
		d_mapFileAlteration.readMapFile();
		
		return this.d_mapFileAlteration.validateMap();
		
	}
	
	
	private void afterCommitReinforcement() {
		
	}
	
	

	public ResponseWrapper addPlayerInGame(String playerName) {
		
		return this.gameModelAlteration.addPlayerInGame(playerName);
	}


	public ResponseWrapper removeplayerFromGame(String playerName) {
		
		return this.gameModelAlteration.removeplayerFromGame(playerName);
	}

	public ResponseWrapper assignCountries() {
		
		return this.gameModelAlteration.assignCountries();
	}
	
	

}

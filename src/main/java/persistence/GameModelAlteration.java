package persistence;

import model.GameModel;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;

public class GameModelAlteration {
	
	private GameModel gameModel;
	private MapModel mapModel;
	
	public GameModelAlteration() {
		gameModel = GameModel.getInstance();
		mapModel = MapModel.getInstance();
	}

	public ResponseWrapper addPlayerInGame(String playerName) {
		Player player = new Player(playerName);
		this.gameModel.addPlayerInPlayersList(player);
		
		return new ResponseWrapper(200,"Player added successfully : " + playerName);
		
	}
	public ResponseWrapper removeplayerFromGame(String playerName) {
		
		for(Player player: this.gameModel.getPlayers()) {
			if(player.getPlayerName().equals(playerName)) {
				this.gameModel.getPlayers().remove(player);
				break;
			}
		}
		return new ResponseWrapper(200,"Player removed successfully");
	}
	public ResponseWrapper assignCountries() {
		
		for(int assignCountry = 0 ; assignCountry < this.gameModel.getPlayers().size() ; assignCountry++) {
			this.gameModel.getPlayers().get(assignCountry).addCountryHold(this.mapModel.getCountries().get(assignCountry));
		}
		
		return new ResponseWrapper(200, "Countries Assigned");
		
	}
}

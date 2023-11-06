package persistence;

import model.*;

import java.util.Collections;
import java.util.List;

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
		
		return new ResponseWrapper(200,"Player added successfully: " + playerName);
		
	}
	public ResponseWrapper removePlayerFromGame(String playerName) {
		
		for(Player player: this.gameModel.getPlayers()) {
			if(player.getPlayerName().equals(playerName)) {
				this.gameModel.getPlayers().remove(player);
				break;
			}
		}
		return new ResponseWrapper(200,"Player removed successfully.");
	}
	public ResponseWrapper assignCountries() {
		int index = 0;
		List<Player> l_inGamePlayers = this.gameModel.getPlayers();

		List<Country> l_availableCountriesList = this.mapModel.getCountries();
		Collections.shuffle(l_availableCountriesList);

		for (Country l_Country : l_availableCountriesList) {
			Player l_Player = l_inGamePlayers.get(index);
			l_Player.addCountryHold(l_Country);
			l_Country.setCountryOwner(l_Player);
			
			if (index < this.gameModel.getPlayers().size() - 1) {
				index++;
			} else {
				index = 0;
			}
		}
		
		return new ResponseWrapper(200, "Countries Assigned");
		
	}
}

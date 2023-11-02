package business.Order;

import model.Country;
import model.Player;

/**
 * 
 * @author Kevin
 * @version build 2
 */
public class BlockadeOrder implements Order{

	private Country targetCountry;
	private Player player;
	
	public BlockadeOrder(Player p_player, Country p_targetCountry) {
		super();
		player = p_player;
		targetCountry = p_targetCountry;
	}
	
	
	@Override
	public void execute() {

		player.removeCountryHold(targetCountry);
		
		int doubleArmies = 3 * targetCountry.getArmies();
		
		targetCountry.setArmy(doubleArmies);
		
		targetCountry.setNeutral();
		
	}

	@Override
	public boolean valid() {

		if(targetCountry == null) {
			return false;
		}
		else if(player.getCountriesHold().contains(targetCountry)) {
			return true;
		}
		
		return false;
	}

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}
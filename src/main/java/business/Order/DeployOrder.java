package business.Order;

import model.Country;
import model.Player;

public class DeployOrder implements Order{
	
	

	private Country targetCountry;
	private int to_deploy_armies;
	private Player player;

	
	public DeployOrder(Country targetCountry, int to_deploy_armies, Player player) {
		super();
		this.targetCountry = targetCountry;
		this.to_deploy_armies = to_deploy_armies;
		this.player = player;
	}
	
	@Override
	public void execute() {
		if(player.getCurrentArmies() > to_deploy_armies) {
			System.out.println("armies are less in your poll");
		}
		player.setCurrentArmies(player.getCurrentArmies()-to_deploy_armies);
		player.addCountryHold(targetCountry);
		//player.getCurrentArmyInCountry().put(targetCountry, to_deploy_armies);
		
		targetCountry.armiesDeploy(to_deploy_armies);
		
		
		
		
	}

	@Override
	public boolean valid() {
		
		if(targetCountry == null || to_deploy_armies > player.getCurrentArmies() || to_deploy_armies <= 0 ) {
			return false;
		}
		else if(player.getCountriesHold().contains(targetCountry)) {
			return true;
		}
		
		return false;
		
	}

	@Override
	public void printOrder() {
		System.out.println(player.getPlayerName() + " deployed " + to_deploy_armies + " armies at " + targetCountry.getCountryId());
		System.out.println("***************************************");
	}

}
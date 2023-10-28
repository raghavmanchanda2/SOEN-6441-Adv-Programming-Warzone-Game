package business.Order;

import model.Country;
import model.Player;

public class AdvanceOrder implements Order{

	private Country fromCountry;
	private Country targetCountry;
	private int to_deploy_armies;
	private Player player;
	

	public AdvanceOrder(Country fromCountry, Country targetCountry, int to_deploy_armies, Player player) {
		super();
		this.fromCountry = fromCountry;
		this.targetCountry = targetCountry;
		this.to_deploy_armies = to_deploy_armies;
		this.player = player;
	}
	
	@Override
	public void execute() {
		
	}

	@Override
	public boolean valid() {
		
		for(Country country : this.player.getCountriesHold()) {
			System.out.print(country.getCountryId()  );
			for(Country countryi : country.getNeighbors()) {
				System.out.println("n                      " + countryi.getCountryId());
			}
			if(this.fromCountry.equals(country) && country.getNeighbors().contains(targetCountry) ) {
				return true;
			}
		}
		
		return false;
	}


	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}

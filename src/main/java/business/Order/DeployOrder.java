package business.Order;

import model.Country;
import model.Player;

public class DeployOrder implements Order{
	
	private Country targetCountry;
	private int to_deploy_armies;
	private Player player;

	@Override
	public void execute() {
		
	}

	@Override
	public boolean valid() {
		// add validity
		
		return false;
	}

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}
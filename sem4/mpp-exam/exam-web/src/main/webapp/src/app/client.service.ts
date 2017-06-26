import { Injectable } from '@angular/core';

import { Client } from './client';
import { CLIENTS } from './mock-clients';


@Injectable()
export class ClientService {
  getClients(): Promise<Client[]> {
    return Promise.resolve(CLIENTS);
  }

  getClientsSlowly(): Promise<Client[]> {
    return new Promise(resolve => {
      // Simulate server latency with 200 ms delay
      setTimeout(() => resolve(this.getClients()), 200);
    });
  }
}

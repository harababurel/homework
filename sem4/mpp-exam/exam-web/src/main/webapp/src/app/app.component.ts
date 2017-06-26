import { Component } from '@angular/core';
import { Client } from './client';
import { ClientDetailComponent } from './client-detail.component';
import { ClientService } from './client.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
  providers: [ClientService],
})
export class AppComponent {
  title = 'MPP Exam';

  clients: Client[];
  selectedClient: Client;

  onSelect(client: Client): void {
    this.selectedClient = client;
  };

  constructor(private clientService: ClientService) {
  }

  getClients(): void {
    this.clientService.getClientsSlowly().then(clients => this.clients = clients);
  }

  ngOnInit(): void {
    this.getClients();
  }
}

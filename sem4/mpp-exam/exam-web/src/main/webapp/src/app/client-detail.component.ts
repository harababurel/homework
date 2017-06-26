import { Component, Input } from '@angular/core';
import { Client } from './client';

@Component({
  selector: 'client-detail',
  template: `
    <div *ngIf="client">
      <h2>{{client.name}} details!</h2>
      <div>
        <label>id: </label>{{client.id}}
      </div>
      <div>
          <label>name: </label>
          <input [(ngModel)]="client.name" placeholder="name"/>
      </div>
    </div>
  `
})

export class ClientDetailComponent {
  @Input() client: Client;
}

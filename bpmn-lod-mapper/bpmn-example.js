export const bpmn = `
<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL" xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI" xmlns:dc="http://www.omg.org/spec/DD/20100524/DC" xmlns:di="http://www.omg.org/spec/DD/20100524/DI" xmlns:modeler="http://camunda.org/schema/modeler/1.0" id="Definitions_0xlkhhp" targetNamespace="http://bpmn.io/schema/bpmn" exporter="Camunda Modeler" exporterVersion="5.16.0" modeler:executionPlatform="Camunda Cloud" modeler:executionPlatformVersion="8.3.0">
  <bpmn:process id="Process_0bmex78" isExecutable="true">
    <bpmn:startEvent id="StartEvent_1">
      <bpmn:outgoing>Flow_0nbpq1a</bpmn:outgoing>
    </bpmn:startEvent>
    <bpmn:task id="Activity_1jtx96i" name="activity-1">
      <bpmn:incoming>Flow_0nbpq1a</bpmn:incoming>
      <bpmn:outgoing>Flow_0dw9g69</bpmn:outgoing>
    </bpmn:task>
    <bpmn:sequenceFlow id="Flow_0nbpq1a" sourceRef="StartEvent_1" targetRef="Activity_1jtx96i" />
    <bpmn:exclusiveGateway id="Gateway_1d0exz5" name="gateway-1">
      <bpmn:incoming>Flow_0dw9g69</bpmn:incoming>
      <bpmn:outgoing>Flow_1490u8k</bpmn:outgoing>
      <bpmn:outgoing>Flow_0gt5lbb</bpmn:outgoing>
    </bpmn:exclusiveGateway>
    <bpmn:sequenceFlow id="Flow_0dw9g69" sourceRef="Activity_1jtx96i" targetRef="Gateway_1d0exz5" />
    <bpmn:task id="Activity_01recz5" name="activity-2">
      <bpmn:incoming>Flow_1490u8k</bpmn:incoming>
      <bpmn:outgoing>Flow_1u0454e</bpmn:outgoing>
    </bpmn:task>
    <bpmn:sequenceFlow id="Flow_1490u8k" name="yes" sourceRef="Gateway_1d0exz5" targetRef="Activity_01recz5" />
    <bpmn:task id="Activity_1nm0k0b" name="activity-3">
      <bpmn:incoming>Flow_0gt5lbb</bpmn:incoming>
      <bpmn:outgoing>Flow_07it9sn</bpmn:outgoing>
    </bpmn:task>
    <bpmn:sequenceFlow id="Flow_0gt5lbb" name="no" sourceRef="Gateway_1d0exz5" targetRef="Activity_1nm0k0b" />
    <bpmn:exclusiveGateway id="Gateway_02c8x7p">
      <bpmn:incoming>Flow_1u0454e</bpmn:incoming>
      <bpmn:incoming>Flow_07it9sn</bpmn:incoming>
      <bpmn:outgoing>Flow_19trwid</bpmn:outgoing>
    </bpmn:exclusiveGateway>
    <bpmn:sequenceFlow id="Flow_1u0454e" sourceRef="Activity_01recz5" targetRef="Gateway_02c8x7p" />
    <bpmn:sequenceFlow id="Flow_07it9sn" sourceRef="Activity_1nm0k0b" targetRef="Gateway_02c8x7p" />
    <bpmn:endEvent id="Event_08tckqm">
      <bpmn:incoming>Flow_19trwid</bpmn:incoming>
    </bpmn:endEvent>
    <bpmn:sequenceFlow id="Flow_19trwid" sourceRef="Gateway_02c8x7p" targetRef="Event_08tckqm" />
  </bpmn:process>
  <bpmndi:BPMNDiagram id="BPMNDiagram_1">
    <bpmndi:BPMNPlane id="BPMNPlane_1" bpmnElement="Process_0bmex78">
      <bpmndi:BPMNShape id="_BPMNShape_StartEvent_2" bpmnElement="StartEvent_1">
        <dc:Bounds x="152" y="122" width="36" height="36" />
      </bpmndi:BPMNShape>
      <bpmndi:BPMNShape id="Activity_1jtx96i_di" bpmnElement="Activity_1jtx96i">
        <dc:Bounds x="240" y="100" width="100" height="80" />
        <bpmndi:BPMNLabel />
      </bpmndi:BPMNShape>
      <bpmndi:BPMNShape id="Gateway_1d0exz5_di" bpmnElement="Gateway_1d0exz5" isMarkerVisible="true">
        <dc:Bounds x="395" y="115" width="50" height="50" />
        <bpmndi:BPMNLabel>
          <dc:Bounds x="395" y="85" width="51" height="14" />
        </bpmndi:BPMNLabel>
      </bpmndi:BPMNShape>
      <bpmndi:BPMNShape id="Activity_01recz5_di" bpmnElement="Activity_01recz5">
        <dc:Bounds x="500" y="100" width="100" height="80" />
        <bpmndi:BPMNLabel />
      </bpmndi:BPMNShape>
      <bpmndi:BPMNShape id="Activity_1nm0k0b_di" bpmnElement="Activity_1nm0k0b">
        <dc:Bounds x="500" y="210" width="100" height="80" />
        <bpmndi:BPMNLabel />
      </bpmndi:BPMNShape>
      <bpmndi:BPMNShape id="Gateway_02c8x7p_di" bpmnElement="Gateway_02c8x7p" isMarkerVisible="true">
        <dc:Bounds x="655" y="115" width="50" height="50" />
      </bpmndi:BPMNShape>
      <bpmndi:BPMNShape id="Event_08tckqm_di" bpmnElement="Event_08tckqm">
        <dc:Bounds x="762" y="122" width="36" height="36" />
      </bpmndi:BPMNShape>
      <bpmndi:BPMNEdge id="Flow_0nbpq1a_di" bpmnElement="Flow_0nbpq1a">
        <di:waypoint x="188" y="140" />
        <di:waypoint x="240" y="140" />
      </bpmndi:BPMNEdge>
      <bpmndi:BPMNEdge id="Flow_0dw9g69_di" bpmnElement="Flow_0dw9g69">
        <di:waypoint x="340" y="140" />
        <di:waypoint x="395" y="140" />
      </bpmndi:BPMNEdge>
      <bpmndi:BPMNEdge id="Flow_1490u8k_di" bpmnElement="Flow_1490u8k">
        <di:waypoint x="445" y="140" />
        <di:waypoint x="500" y="140" />
        <bpmndi:BPMNLabel>
          <dc:Bounds x="464" y="122" width="17" height="14" />
        </bpmndi:BPMNLabel>
      </bpmndi:BPMNEdge>
      <bpmndi:BPMNEdge id="Flow_0gt5lbb_di" bpmnElement="Flow_0gt5lbb">
        <di:waypoint x="420" y="165" />
        <di:waypoint x="420" y="250" />
        <di:waypoint x="500" y="250" />
        <bpmndi:BPMNLabel>
          <dc:Bounds x="429" y="205" width="13" height="14" />
        </bpmndi:BPMNLabel>
      </bpmndi:BPMNEdge>
      <bpmndi:BPMNEdge id="Flow_1u0454e_di" bpmnElement="Flow_1u0454e">
        <di:waypoint x="600" y="140" />
        <di:waypoint x="655" y="140" />
      </bpmndi:BPMNEdge>
      <bpmndi:BPMNEdge id="Flow_07it9sn_di" bpmnElement="Flow_07it9sn">
        <di:waypoint x="600" y="250" />
        <di:waypoint x="680" y="250" />
        <di:waypoint x="680" y="165" />
      </bpmndi:BPMNEdge>
      <bpmndi:BPMNEdge id="Flow_19trwid_di" bpmnElement="Flow_19trwid">
        <di:waypoint x="705" y="140" />
        <di:waypoint x="762" y="140" />
      </bpmndi:BPMNEdge>
    </bpmndi:BPMNPlane>
  </bpmndi:BPMNDiagram>
</bpmn:definitions>
`;

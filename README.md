# marketing_messages_measurement
This project provides best practices on how to measure WhatsApp marketing messages effectively, understand how many incremental 
conversions businesses canget as well as how to compare the effectiveness of marketing messages against other external platforms such as email/SMS.

## Summarized testing steps
1. Pick a business use case to test. For example, a finance institution could create a campaign to offer a new credit card for current customers.
   The success metric here would be credit card signups from current customers.
3. Define if the WhatsApp marketing messages test will also contain benchmarks against other external platforms such as email or SMS.
   The experimental design will be based on how many benchmarks will be tested. The treatment groups will be:
    a. Control: customers that will not receive any message
    b. WhatsApp marketing message: customers that will receive the marketing communication
    c. Other cells for additional channel(s) comparison
5. Select metrics that will be important for assessing the effectiveness of marketing messages.
   Ultimately, the most important metrics are incremental conversions and cost per incremental conversions, but other upper funnel metrics can be tracked.
7. Use audience sizes and conversions for previous WhatsApp marketing messages campaigns and perform a power calculation to be able to
   detect at least a 10% increase in conversions between control and treated groups. Different percentage increases can be defined if appropriate.
    a. When comparing WhatsApp marketing messages vs. a holdout (Type A analysis), the power calculation can focus on detecting changes on conversion rates only.
    b. When comparing between external platforms/benchmarks (Type B analysis), the power calculation analysis should focus on improvements on cost per action (Conversion, CPA).
9. Execute the test, monitoring messages delivered in each treatment cell to calculate costs appropriately.
10. Track conversions from each treatment group.
11. Calculate test results metrics:
    a. Number of conversions between control and treatment groups
    b. Cost per incremental conversions, if the test is against holdout only
    c. Average improvement in CPA (cost per action) if comparing WhatsApp marketing messages with external platforms (email/SMS)
12. Discuss if final results are good enough to incentivize broad adoption of WhatsApp marketing messages or if any experiment iteration is necessary
    (test new message, new frequency, new benchmark channels, etc).

See the [CONTRIBUTING] file for how to help out.

## License
marketing_messages_measurement is MIT licensed, as found in the LICENSE file.
